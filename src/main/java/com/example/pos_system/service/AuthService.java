package com.example.pos_system.service;

import com.example.pos_system.entity.AppUser;
import com.example.pos_system.entity.Permission;
import com.example.pos_system.entity.UserAuditLog;
import com.example.pos_system.entity.UserRole;
import com.example.pos_system.repository.AppUserRepo;
import com.example.pos_system.repository.UserAuditLogRepo;
import io.jsonwebtoken.Claims;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

@Service
@Transactional
public class AuthService {
    private static final Pattern EMAIL_PATTERN =
            Pattern.compile("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$", Pattern.CASE_INSENSITIVE);

    private final AppUserRepo appUserRepo;
    private final PasswordEncoder passwordEncoder;
    private final RolePermissionService rolePermissionService;
    private final SpeakeasyTotpService speakeasyTotpService;
    private final JwtTokenService jwtTokenService;
    private final UserAuditLogRepo userAuditLogRepo;
    private final AuditEventService auditEventService;

    @Value("${app.security.login.max-failed-attempts:5}")
    private int maxFailedAttempts;

    @Value("${app.security.login.lock-duration-minutes:15}")
    private int lockDurationMinutes;

    @Value("${app.security.jwt.access-token-minutes:720}")
    private long accessTokenMinutes;

    /**
     * Executes the AuthService operation.
     * <p>Return value: A fully initialized AuthService instance.</p>
     *
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * @param passwordEncoder Parameter of type {@code PasswordEncoder} used by this operation.
     * @param rolePermissionService Parameter of type {@code RolePermissionService} used by this operation.
     * @param speakeasyTotpService Parameter of type {@code SpeakeasyTotpService} used by this operation.
     * @param jwtTokenService Parameter of type {@code JwtTokenService} used by this operation.
     * @param userAuditLogRepo Parameter of type {@code UserAuditLogRepo} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public AuthService(AppUserRepo appUserRepo,
                       PasswordEncoder passwordEncoder,
                       RolePermissionService rolePermissionService,
                       SpeakeasyTotpService speakeasyTotpService,
                       JwtTokenService jwtTokenService,
                       UserAuditLogRepo userAuditLogRepo,
                       AuditEventService auditEventService) {
        this.appUserRepo = appUserRepo;
        this.passwordEncoder = passwordEncoder;
        this.rolePermissionService = rolePermissionService;
        this.speakeasyTotpService = speakeasyTotpService;
        this.jwtTokenService = jwtTokenService;
        this.userAuditLogRepo = userAuditLogRepo;
        this.auditEventService = auditEventService;
    }

    /**
     * Executes the register operation.
     *
     * @param email Parameter of type {@code String} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param roleName Parameter of type {@code String} used by this operation.
     * @return {@code RegisterResult} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public RegisterResult register(String email, String password, String roleName) {
        String normalizedEmail = normalizeEmail(email);
        validatePassword(password);
        if (appUserRepo.existsByEmailIgnoreCase(normalizedEmail)) {
            throw new ResponseStatusException(HttpStatus.CONFLICT, "Email already registered.");
        }

        UserRole role = resolveRole(roleName);
        String username = generateUsernameFromEmail(normalizedEmail);
        Set<Permission> permissions = rolePermissionService.defaultsForRole(role);

        AppUser user = new AppUser();
        user.setUsername(username);
        user.setEmail(normalizedEmail);
        user.setPassword(passwordEncoder.encode(password));
        user.setRole(role);
        user.setActive(true);
        user.setMfaRequired(true);
        user.setPermissions(permissions);
        AppUser saved = appUserRepo.save(user);

        recordAudit(saved.getUsername(), saved.getUsername(), "REGISTER",
                "Registered via API with role " + role.name());
        auditEventService.record(
                "AUTH_REGISTER",
                "AUTH",
                saved.getId(),
                null,
                authSnapshot(saved),
                Map.of("email", normalizedEmail, "role", role.name())
        );

        return new RegisterResult(
                saved.getId(),
                saved.getUsername(),
                saved.getEmail(),
                saved.getRole(),
                permissionNames(saved.getPermissions())
        );
    }

    /**
     * Executes the loginWithPassword operation.
     *
     * @param email Parameter of type {@code String} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code LoginResult} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public LoginResult loginWithPassword(String email, String password, HttpServletRequest request) {
        String normalizedEmail = normalizeEmail(email);
        AppUser user = appUserRepo.findByEmailIgnoreCase(normalizedEmail).orElse(null);

        if (user == null) {
            recordAudit(normalizedEmail, normalizedEmail, "LOGIN_FAILED", "Unknown email.");
            auditEventService.record("AUTH_LOGIN_FAILED", "AUTH", null, null, null,
                    Map.of("reason", "unknown_email", "email", normalizedEmail, "ip", extractIp(request)));
            return LoginResult.invalidCredentials();
        }

        if (!Boolean.TRUE.equals(user.getActive())) {
            recordAudit(user.getUsername(), user.getUsername(), "LOGIN_FAILED", "Account disabled.");
            auditEventService.record("AUTH_LOGIN_FAILED", "AUTH", user.getId(), null, null,
                    Map.of("reason", "disabled", "email", normalizedEmail, "ip", extractIp(request)));
            return LoginResult.disabled();
        }

        if (isLocked(user)) {
            long remaining = remainingMinutes(user.getLockedUntil());
            recordAudit(user.getUsername(), user.getUsername(), "LOGIN_FAILED",
                    "Account locked. remainingMinutes=" + remaining);
            return LoginResult.locked(remaining);
        }

        if (!passwordEncoder.matches(password == null ? "" : password, user.getPassword())) {
            return onFailedAttempt(user, "invalid_password", request);
        }

        clearFailures(user);

        boolean firstTimeSetup = !Boolean.TRUE.equals(user.getTotpEnabled())
                || user.getTotpSecret() == null
                || user.getTotpSecret().isBlank();
        String otpauthUrl = null;
        String qrDataUrl = null;

        if (firstTimeSetup) {
            SpeakeasyTotpService.SetupPayload payload;
            try {
                payload = speakeasyTotpService.generateSetup(user.getEmail());
            } catch (IllegalStateException ex) {
                throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE,
                        "TOTP service unavailable. Ensure Node.js and speakeasy dependencies are installed.");
            }
            user.setTotpSecret(payload.base32Secret());
            user.setTotpEnabled(false);
            otpauthUrl = payload.otpauthUrl();
            qrDataUrl = payload.qrDataUrl();
            appUserRepo.save(user);
        }

        String challengeToken = jwtTokenService.issueOtpChallengeToken(user);

        recordAudit(user.getUsername(), user.getUsername(), "LOGIN_PASSWORD_OK", "Password accepted, OTP required.");
        auditEventService.record("AUTH_LOGIN_PASSWORD_OK", "AUTH", user.getId(), null, null,
                Map.of("firstTimeTotpSetup", firstTimeSetup, "email", normalizedEmail, "ip", extractIp(request)));

        return LoginResult.otpRequired(challengeToken, firstTimeSetup, otpauthUrl, qrDataUrl);
    }

    /**
     * Executes the verifyOtp operation.
     *
     * @param challengeToken Parameter of type {@code String} used by this operation.
     * @param otpCode Parameter of type {@code String} used by this operation.
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code OtpResult} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public OtpResult verifyOtp(String challengeToken, String otpCode, HttpServletRequest request) {
        Claims claims = jwtTokenService.parseOtpChallengeToken(challengeToken);
        Long userId = parseUserId(claims.getSubject());
        AppUser user = appUserRepo.findById(userId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Invalid challenge token."));

        if (!Boolean.TRUE.equals(user.getActive())) {
            return OtpResult.disabled();
        }

        if (isLocked(user)) {
            return OtpResult.locked(remainingMinutes(user.getLockedUntil()));
        }

        boolean validOtp;
        try {
            validOtp = speakeasyTotpService.verifyCode(user.getTotpSecret(), otpCode);
        } catch (IllegalStateException ex) {
            throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE,
                    "TOTP verification service unavailable. Ensure Node.js and speakeasy dependencies are installed.");
        }

        if (!validOtp) {
            LoginResult failed = onFailedAttempt(user, "invalid_totp", request);
            if (failed.status() == LoginStatus.LOCKED) {
                return OtpResult.locked(failed.lockedMinutes());
            }
            return OtpResult.invalidOtp();
        }

        clearFailures(user);
        user.setTotpEnabled(true);
        user.setLastTotpVerifiedAt(LocalDateTime.now());
        user.setLastLoginAt(LocalDateTime.now());
        appUserRepo.save(user);

        String accessToken = jwtTokenService.issueAccessToken(user);
        long expiresInSeconds = Duration.ofMinutes(normalizedMinutes(accessTokenMinutes)).toSeconds();

        recordAudit(user.getUsername(), user.getUsername(), "LOGIN_SUCCESS", "OTP verified, JWT issued.");
        auditEventService.record("AUTH_LOGIN_SUCCESS", "AUTH", user.getId(), null, null,
                Map.of("email", user.getEmail(), "ip", extractIp(request)));

        return OtpResult.success(
                accessToken,
                expiresInSeconds,
                user.getRole(),
                permissionNames(user.getPermissions())
        );
    }

    /**
     * Executes the onFailedAttempt operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param reason Parameter of type {@code String} used by this operation.
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code LoginResult} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private LoginResult onFailedAttempt(AppUser user, String reason, HttpServletRequest request) {
        LocalDateTime now = LocalDateTime.now();
        int attempts = safeAttempts(user) + 1;
        user.setFailedLoginAttempts(attempts);
        user.setLastFailedLoginAt(now);

        boolean locked = attempts >= normalizedMaxAttempts();
        long remainingMinutes = 0L;
        if (locked) {
            user.setLockedUntil(now.plusMinutes(normalizedMinutes(lockDurationMinutes)));
            remainingMinutes = remainingMinutes(user.getLockedUntil());
        }
        appUserRepo.save(user);

        String detail = "Login failed (" + reason + "), attempts=" + attempts;
        if (locked) {
            detail += ", lockedMinutes=" + remainingMinutes;
        }
        recordAudit(user.getUsername(), user.getUsername(), "LOGIN_FAILED", detail);

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("reason", reason);
        metadata.put("attempts", attempts);
        metadata.put("locked", locked);
        metadata.put("lockedUntil", user.getLockedUntil());
        metadata.put("ip", extractIp(request));
        metadata.put("email", user.getEmail());
        auditEventService.record("AUTH_LOGIN_FAILED", "AUTH", user.getId(), null, null, metadata);

        if (locked) {
            return LoginResult.locked(remainingMinutes);
        }
        return LoginResult.invalidCredentials();
    }

    /**
     * Executes the clearFailures operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void clearFailures(AppUser user) {
        if (user == null) {
            return;
        }
        user.setFailedLoginAttempts(0);
        user.setLastFailedLoginAt(null);
        user.setLockedUntil(null);
        appUserRepo.save(user);
    }

    /**
     * Executes the isLocked operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean isLocked(AppUser user) {
        if (user == null || user.getLockedUntil() == null) {
            return false;
        }
        return user.getLockedUntil().isAfter(LocalDateTime.now());
    }

    /**
     * Executes the remainingMinutes operation.
     *
     * @param lockedUntil Parameter of type {@code LocalDateTime} used by this operation.
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private long remainingMinutes(LocalDateTime lockedUntil) {
        if (lockedUntil == null) {
            return normalizedMinutes(lockDurationMinutes);
        }
        long minutes = Duration.between(LocalDateTime.now(), lockedUntil).toMinutes();
        return Math.max(1, minutes);
    }

    /**
     * Executes the normalizeEmail operation.
     *
     * @param email Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalizeEmail(String email) {
        String normalized = email == null ? "" : email.trim().toLowerCase(Locale.ROOT);
        if (normalized.isEmpty() || !EMAIL_PATTERN.matcher(normalized).matches()) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "A valid email is required.");
        }
        return normalized;
    }

    /**
     * Executes the validatePassword operation.
     *
     * @param password Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void validatePassword(String password) {
        if (password == null || password.length() < 8) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Password must be at least 8 characters.");
        }
    }

    /**
     * Executes the resolveRole operation.
     *
     * @param roleName Parameter of type {@code String} used by this operation.
     * @return {@code UserRole} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private UserRole resolveRole(String roleName) {
        if (roleName == null || roleName.isBlank()) {
            return UserRole.CASHIER;
        }
        String normalized = roleName.trim().toUpperCase(Locale.ROOT);
        return switch (normalized) {
            case "SUPER_ADMIN", "ADMIN" -> UserRole.SUPER_ADMIN;
            case "BRANCH_MANAGER", "MANAGER" -> UserRole.BRANCH_MANAGER;
            case "INVENTORY_STAFF" -> UserRole.INVENTORY_STAFF;
            case "CASHIER" -> UserRole.CASHIER;
            default -> throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Unsupported role: " + roleName);
        };
    }

    /**
     * Executes the generateUsernameFromEmail operation.
     *
     * @param email Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String generateUsernameFromEmail(String email) {
        String localPart = email.substring(0, email.indexOf('@')).trim().toLowerCase(Locale.ROOT);
        String base = localPart.replaceAll("[^a-z0-9._-]", ".");
        base = base.replaceAll("\\.+", ".");
        if (base.isBlank()) {
            base = "user";
        }

        String candidate = base;
        int counter = 1;
        while (appUserRepo.existsByUsernameIgnoreCase(candidate)) {
            counter++;
            candidate = base + counter;
        }
        return candidate;
    }

    /**
     * Executes the recordAudit operation.
     *
     * @param actorUsername Parameter of type {@code String} used by this operation.
     * @param targetUsername Parameter of type {@code String} used by this operation.
     * @param action Parameter of type {@code String} used by this operation.
     * @param details Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void recordAudit(String actorUsername, String targetUsername, String action, String details) {
        UserAuditLog log = new UserAuditLog();
        log.setActorUsername(actorUsername);
        log.setTargetUsername(targetUsername);
        log.setAction(action);
        log.setDetails(details);
        userAuditLogRepo.save(log);
    }

    /**
     * Executes the authSnapshot operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @return {@code Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, Object> authSnapshot(AppUser user) {
        Map<String, Object> snapshot = new LinkedHashMap<>();
        snapshot.put("id", user.getId());
        snapshot.put("username", user.getUsername());
        snapshot.put("email", user.getEmail());
        snapshot.put("role", user.getRole() == null ? null : user.getRole().name());
        snapshot.put("permissions", permissionNames(user.getPermissions()));
        return snapshot;
    }

    /**
     * Executes the permissionNames operation.
     *
     * @param permissions Parameter of type {@code Set<Permission>} used by this operation.
     * @return {@code List<String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<String> permissionNames(Set<Permission> permissions) {
        if (permissions == null || permissions.isEmpty()) {
            return List.of();
        }
        return permissions.stream().map(Enum::name).sorted().toList();
    }

    /**
     * Executes the safeAttempts operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int safeAttempts(AppUser user) {
        if (user == null || user.getFailedLoginAttempts() == null) {
            return 0;
        }
        return Math.max(0, user.getFailedLoginAttempts());
    }

    /**
     * Executes the normalizedMaxAttempts operation.
     *
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int normalizedMaxAttempts() {
        return Math.max(1, maxFailedAttempts);
    }

    /**
     * Executes the normalizedMinutes operation.
     *
     * @param configured Parameter of type {@code long} used by this operation.
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private long normalizedMinutes(long configured) {
        return configured <= 0 ? 5 : configured;
    }

    /**
     * Executes the parseUserId operation.
     *
     * @param subject Parameter of type {@code String} used by this operation.
     * @return {@code Long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Long parseUserId(String subject) {
        if (subject == null || subject.isBlank()) {
            throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Invalid challenge token.");
        }
        try {
            return Long.parseLong(subject);
        } catch (NumberFormatException ex) {
            throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Invalid challenge token.");
        }
    }

    /**
     * Executes the extractIp operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String extractIp(HttpServletRequest request) {
        if (request == null) {
            return null;
        }
        String forwarded = request.getHeader("X-Forwarded-For");
        if (forwarded != null && !forwarded.isBlank()) {
            int comma = forwarded.indexOf(',');
            return comma >= 0 ? forwarded.substring(0, comma).trim() : forwarded.trim();
        }
        String realIp = request.getHeader("X-Real-IP");
        if (realIp != null && !realIp.isBlank()) {
            return realIp.trim();
        }
        return request.getRemoteAddr();
    }

    public record RegisterResult(Long userId,
                                 String username,
                                 String email,
                                 UserRole role,
                                 List<String> permissions) {
    }

    public record LoginResult(LoginStatus status,
                              String challengeToken,
                              boolean firstTimeSetup,
                              String otpauthUrl,
                              String qrDataUrl,
                              Long lockedMinutes) {
        /**
         * Executes the invalidCredentials operation.
         *
         * @return {@code LoginResult} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private static LoginResult invalidCredentials() {
            return new LoginResult(LoginStatus.INVALID_CREDENTIALS, null, false, null, null, null);
        }

        /**
         * Executes the disabled operation.
         *
         * @return {@code LoginResult} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private static LoginResult disabled() {
            return new LoginResult(LoginStatus.DISABLED, null, false, null, null, null);
        }

        /**
         * Executes the locked operation.
         *
         * @param lockedMinutes Parameter of type {@code long} used by this operation.
         * @return {@code LoginResult} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private static LoginResult locked(long lockedMinutes) {
            return new LoginResult(LoginStatus.LOCKED, null, false, null, null, lockedMinutes);
        }

        /**
         * Executes the otpRequired operation.
         *
         * @param challengeToken Parameter of type {@code String} used by this operation.
         * @param firstTimeSetup Parameter of type {@code boolean} used by this operation.
         * @param otpauthUrl Parameter of type {@code String} used by this operation.
         * @param qrDataUrl Parameter of type {@code String} used by this operation.
         * @return {@code LoginResult} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private static LoginResult otpRequired(String challengeToken,
                                               boolean firstTimeSetup,
                                               String otpauthUrl,
                                               String qrDataUrl) {
            return new LoginResult(LoginStatus.OTP_REQUIRED, challengeToken, firstTimeSetup, otpauthUrl, qrDataUrl, null);
        }
    }

    public record OtpResult(OtpStatus status,
                            String accessToken,
                            long expiresInSeconds,
                            UserRole role,
                            List<String> permissions,
                            Long lockedMinutes) {
        /**
         * Executes the invalidOtp operation.
         *
         * @return {@code OtpResult} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private static OtpResult invalidOtp() {
            return new OtpResult(OtpStatus.INVALID_OTP, null, 0, null, List.of(), null);
        }

        /**
         * Executes the disabled operation.
         *
         * @return {@code OtpResult} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private static OtpResult disabled() {
            return new OtpResult(OtpStatus.DISABLED, null, 0, null, List.of(), null);
        }

        /**
         * Executes the locked operation.
         *
         * @param lockedMinutes Parameter of type {@code long} used by this operation.
         * @return {@code OtpResult} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private static OtpResult locked(long lockedMinutes) {
            return new OtpResult(OtpStatus.LOCKED, null, 0, null, List.of(), lockedMinutes);
        }

        /**
         * Executes the success operation.
         *
         * @param accessToken Parameter of type {@code String} used by this operation.
         * @param expiresInSeconds Parameter of type {@code long} used by this operation.
         * @param role Parameter of type {@code UserRole} used by this operation.
         * @param permissions Parameter of type {@code List<String>} used by this operation.
         * @return {@code OtpResult} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private static OtpResult success(String accessToken,
                                         long expiresInSeconds,
                                         UserRole role,
                                         List<String> permissions) {
            return new OtpResult(OtpStatus.SUCCESS, accessToken, expiresInSeconds, role, permissions, null);
        }
    }

    public enum LoginStatus {
        INVALID_CREDENTIALS,
        DISABLED,
        LOCKED,
        OTP_REQUIRED
    }

    public enum OtpStatus {
        INVALID_OTP,
        DISABLED,
        LOCKED,
        SUCCESS
    }
}
