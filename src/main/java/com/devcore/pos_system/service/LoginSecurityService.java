package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.AppUser;
import com.devcore.pos_system.entity.UserAuditLog;
import com.devcore.pos_system.repository.AppUserRepo;
import com.devcore.pos_system.repository.UserAuditLogRepo;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.authentication.LockedException;
import org.springframework.security.authentication.DisabledException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.LinkedHashMap;
import java.util.Map;

@Service
@Transactional
public class LoginSecurityService {
    private static final String REASON_BAD_CREDENTIALS = "bad-credentials";
    private static final String REASON_LOCKED = "locked";
    private static final String REASON_DISABLED = "disabled";

    private final AppUserRepo appUserRepo;
    private final UserAuditLogRepo userAuditLogRepo;
    private final AuditEventService auditEventService;

    @Value("${app.security.login.max-failed-attempts:5}")
    private int maxFailedAttempts;

    @Value("${app.security.login.lock-duration-minutes:15}")
    private int lockDurationMinutes;

    /**
     * Executes the LoginSecurityService operation.
     * <p>Return value: A fully initialized LoginSecurityService instance.</p>
     *
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * @param userAuditLogRepo Parameter of type {@code UserAuditLogRepo} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public LoginSecurityService(AppUserRepo appUserRepo,
                                UserAuditLogRepo userAuditLogRepo,
                                AuditEventService auditEventService) {
        this.appUserRepo = appUserRepo;
        this.userAuditLogRepo = userAuditLogRepo;
        this.auditEventService = auditEventService;
    }

    /**
     * Executes the registerFailure operation.
     *
     * @param rawUsername Parameter of type {@code String} used by this operation.
     * @param exception Parameter of type {@code AuthenticationException} used by this operation.
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code FailureOutcome} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public FailureOutcome registerFailure(String rawUsername,
                                          AuthenticationException exception,
                                          HttpServletRequest request) {
        String username = normalize(rawUsername);
        LocalDateTime now = LocalDateTime.now();
        AppUser user = username == null ? null : appUserRepo.findByUsernameIgnoreCase(username).orElse(null);

        if (user != null) {
            clearExpiredLock(user, now);
        }

        FailureOutcome outcome;
        if (exception instanceof LockedException) {
            LocalDateTime lockedUntil = user == null ? null : user.getLockedUntil();
            outcome = FailureOutcome.locked(lockedUntil);
        } else if (exception instanceof DisabledException) {
            outcome = FailureOutcome.disabled();
        } else {
            outcome = processCredentialFailure(user, now);
        }

        recordFailureAudit(user, username, outcome, request);
        return outcome;
    }

    /**
     * Executes the registerSuccess operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void registerSuccess(String username) {
        String normalized = normalize(username);
        if (normalized == null) {
            return;
        }
        AppUser user = appUserRepo.findByUsernameIgnoreCase(normalized).orElse(null);
        if (user == null) {
            return;
        }

        if (safeAttempts(user) > 0 || user.getLockedUntil() != null || user.getLastFailedLoginAt() != null) {
            user.setFailedLoginAttempts(0);
            user.setLastFailedLoginAt(null);
            user.setLockedUntil(null);
            appUserRepo.save(user);
        }
    }

    /**
     * Executes the isCurrentlyLocked operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public boolean isCurrentlyLocked(AppUser user) {
        return isCurrentlyLocked(user, LocalDateTime.now());
    }

    /**
     * Executes the processCredentialFailure operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param now Parameter of type {@code LocalDateTime} used by this operation.
     * @return {@code FailureOutcome} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private FailureOutcome processCredentialFailure(AppUser user, LocalDateTime now) {
        if (user == null) {
            return FailureOutcome.badCredentials();
        }

        if (isCurrentlyLocked(user, now)) {
            return FailureOutcome.locked(user.getLockedUntil());
        }

        int attempts = safeAttempts(user) + 1;
        user.setFailedLoginAttempts(attempts);
        user.setLastFailedLoginAt(now);

        if (attempts >= maxAllowedAttempts()) {
            user.setLockedUntil(now.plusMinutes(lockMinutes()));
            appUserRepo.save(user);
            return FailureOutcome.locked(user.getLockedUntil());
        }

        appUserRepo.save(user);
        return FailureOutcome.badCredentials();
    }

    /**
     * Executes the clearExpiredLock operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param now Parameter of type {@code LocalDateTime} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void clearExpiredLock(AppUser user, LocalDateTime now) {
        if (user == null) {
            return;
        }
        LocalDateTime lockedUntil = user.getLockedUntil();
        if (lockedUntil != null && !lockedUntil.isAfter(now)) {
            user.setLockedUntil(null);
            user.setFailedLoginAttempts(0);
            appUserRepo.save(user);
        }
    }

    /**
     * Executes the isCurrentlyLocked operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param now Parameter of type {@code LocalDateTime} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean isCurrentlyLocked(AppUser user, LocalDateTime now) {
        if (user == null || user.getLockedUntil() == null) {
            return false;
        }
        return user.getLockedUntil().isAfter(now);
    }

    /**
     * Executes the recordFailureAudit operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param attemptedUsername Parameter of type {@code String} used by this operation.
     * @param outcome Parameter of type {@code FailureOutcome} used by this operation.
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void recordFailureAudit(AppUser user,
                                    String attemptedUsername,
                                    FailureOutcome outcome,
                                    HttpServletRequest request) {
        String safeUsername = attemptedUsername == null ? "anonymous" : attemptedUsername;
        String targetUsername = user == null ? attemptedUsername : user.getUsername();

        UserAuditLog log = new UserAuditLog();
        log.setActorUsername(safeUsername);
        log.setTargetUsername(targetUsername);
        log.setAction("LOGIN_FAILED");
        log.setDetails("reason=" + outcome.reason() + ", ip=" + extractIpAddress(request));
        userAuditLogRepo.save(log);

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("usernameProvided", attemptedUsername != null);
        metadata.put("username", attemptedUsername);
        metadata.put("reason", outcome.reason());
        metadata.put("ip", extractIpAddress(request));
        metadata.put("lockoutUntil", outcome.lockedUntil());
        auditEventService.record(
                "LOGIN_FAILED",
                "AUTH",
                user == null ? null : user.getId(),
                null,
                null,
                metadata
        );
    }

    /**
     * Executes the extractIpAddress operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String extractIpAddress(HttpServletRequest request) {
        if (request == null) {
            return null;
        }
        String forwarded = normalize(request.getHeader("X-Forwarded-For"));
        if (forwarded != null) {
            int commaIndex = forwarded.indexOf(',');
            return commaIndex >= 0 ? forwarded.substring(0, commaIndex).trim() : forwarded;
        }
        String realIp = normalize(request.getHeader("X-Real-IP"));
        if (realIp != null) {
            return realIp;
        }
        return normalize(request.getRemoteAddr());
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
     * Executes the maxAllowedAttempts operation.
     *
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int maxAllowedAttempts() {
        return Math.max(1, maxFailedAttempts);
    }

    /**
     * Executes the lockMinutes operation.
     *
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int lockMinutes() {
        return Math.max(1, lockDurationMinutes);
    }

    /**
     * Executes the normalize operation.
     *
     * @param raw Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalize(String raw) {
        if (raw == null) {
            return null;
        }
        String normalized = raw.trim();
        return normalized.isEmpty() ? null : normalized;
    }

    public record FailureOutcome(String reason, LocalDateTime lockedUntil) {
        /**
         * Executes the badCredentials operation.
         *
         * @return {@code FailureOutcome} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private static FailureOutcome badCredentials() {
            return new FailureOutcome(REASON_BAD_CREDENTIALS, null);
        }

        /**
         * Executes the locked operation.
         *
         * @param lockedUntil Parameter of type {@code LocalDateTime} used by this operation.
         * @return {@code FailureOutcome} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private static FailureOutcome locked(LocalDateTime lockedUntil) {
            return new FailureOutcome(REASON_LOCKED, lockedUntil);
        }

        /**
         * Executes the disabled operation.
         *
         * @return {@code FailureOutcome} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private static FailureOutcome disabled() {
            return new FailureOutcome(REASON_DISABLED, null);
        }
    }
}
