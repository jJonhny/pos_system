package com.example.pos_system.service;

import com.example.pos_system.entity.AppUser;
import com.example.pos_system.entity.Permission;
import com.example.pos_system.entity.UserAuditLog;
import com.example.pos_system.entity.UserRole;
import com.example.pos_system.repository.AppUserRepo;
import com.example.pos_system.repository.UserAuditLogRepo;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.DisabledException;
import org.springframework.security.authentication.LockedException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
@Transactional
public class SsoAuthenticationService {
    private final AppUserRepo appUserRepo;
    private final UserAuditLogRepo userAuditLogRepo;
    private final PasswordEncoder passwordEncoder;
    private final RolePermissionService rolePermissionService;
    private final LoginSecurityService loginSecurityService;
    private final AuditEventService auditEventService;
    private final boolean autoProvision;
    private final UserRole defaultRole;
    private final String emailClaim;
    private final String usernameClaim;

    /**
     * Executes the SsoAuthenticationService operation.
     * <p>Return value: A fully initialized SsoAuthenticationService instance.</p>
     *
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * @param userAuditLogRepo Parameter of type {@code UserAuditLogRepo} used by this operation.
     * @param passwordEncoder Parameter of type {@code PasswordEncoder} used by this operation.
     * @param rolePermissionService Parameter of type {@code RolePermissionService} used by this operation.
     * @param loginSecurityService Parameter of type {@code LoginSecurityService} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * @param autoProvision Parameter of type {@code boolean} used by this operation.
     * @param defaultRole Parameter of type {@code UserRole} used by this operation.
     * @param emailClaim Parameter of type {@code String} used by this operation.
     * @param usernameClaim Parameter of type {@code String} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public SsoAuthenticationService(AppUserRepo appUserRepo,
                                    UserAuditLogRepo userAuditLogRepo,
                                    PasswordEncoder passwordEncoder,
                                    RolePermissionService rolePermissionService,
                                    LoginSecurityService loginSecurityService,
                                    AuditEventService auditEventService,
                                    @Value("${app.auth.sso.auto-provision:true}") boolean autoProvision,
                                    @Value("${app.auth.sso.default-role:CASHIER}") UserRole defaultRole,
                                    @Value("${app.auth.sso.email-claim:email}") String emailClaim,
                                    @Value("${app.auth.sso.username-claim:preferred_username}") String usernameClaim) {
        this.appUserRepo = appUserRepo;
        this.userAuditLogRepo = userAuditLogRepo;
        this.passwordEncoder = passwordEncoder;
        this.rolePermissionService = rolePermissionService;
        this.loginSecurityService = loginSecurityService;
        this.auditEventService = auditEventService;
        this.autoProvision = autoProvision;
        this.defaultRole = defaultRole == null ? UserRole.CASHIER : defaultRole;
        this.emailClaim = emailClaim == null || emailClaim.isBlank() ? "email" : emailClaim.trim();
        this.usernameClaim = usernameClaim == null || usernameClaim.isBlank()
                ? "preferred_username"
                : usernameClaim.trim();
    }

    /**
     * Executes the completeLogin operation.
     *
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @return {@code Authentication} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Authentication completeLogin(Authentication authentication) {
        if (!(authentication instanceof OAuth2AuthenticationToken oauth2Token)) {
            throw new DisabledException("SSO authentication token missing.");
        }
        OAuth2User oauth2User = oauth2Token.getPrincipal();
        Map<String, Object> attributes = oauth2User == null ? Map.of() : oauth2User.getAttributes();
        String provider = oauth2Token.getAuthorizedClientRegistrationId();
        String email = resolveEmail(attributes);
        if (email == null) {
            throw new DisabledException("SSO identity does not include an email.");
        }

        AppUser user = resolveOrProvisionUser(email, attributes);
        if (Boolean.FALSE.equals(user.getActive())) {
            throw new DisabledException("User account is inactive.");
        }
        if (user.getLockedUntil() != null && user.getLockedUntil().isAfter(LocalDateTime.now())) {
            throw new LockedException("User account is temporarily locked.");
        }

        loginSecurityService.registerSuccess(user.getUsername());
        user.setLastLoginAt(LocalDateTime.now());
        AppUser saved = appUserRepo.save(user);

        List<GrantedAuthority> authorities = buildAuthorities(saved);
        UsernamePasswordAuthenticationToken localAuth = UsernamePasswordAuthenticationToken.authenticated(
                saved.getUsername(),
                "N/A",
                authorities
        );
        localAuth.setDetails(authentication.getDetails());

        recordSsoLogin(saved, provider, attributes);
        return localAuth;
    }

    /**
     * Executes the resolveOrProvisionUser operation.
     *
     * @param email Parameter of type {@code String} used by this operation.
     * @param attributes Parameter of type {@code Map<String, Object>} used by this operation.
     * @return {@code AppUser} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private AppUser resolveOrProvisionUser(String email, Map<String, Object> attributes) {
        AppUser byEmail = appUserRepo.findByEmailIgnoreCase(email).orElse(null);
        if (byEmail != null) {
            return byEmail;
        }

        String preferredUsername = normalize(firstNonBlank(
                attributeValue(attributes, usernameClaim),
                attributeValue(attributes, "preferred_username"),
                attributeValue(attributes, "name"),
                email.split("@")[0]
        ));
        if (preferredUsername != null) {
            AppUser byUsername = appUserRepo.findByUsernameIgnoreCase(preferredUsername).orElse(null);
            if (byUsername != null) {
                if (byUsername.getEmail() == null || byUsername.getEmail().isBlank()) {
                    byUsername.setEmail(email);
                    return appUserRepo.save(byUsername);
                }
                return byUsername;
            }
        }

        if (!autoProvision) {
            throw new DisabledException("SSO account is not provisioned.");
        }
        return createProvisionedUser(email, preferredUsername);
    }

    /**
     * Executes the createProvisionedUser operation.
     *
     * @param email Parameter of type {@code String} used by this operation.
     * @param preferredUsername Parameter of type {@code String} used by this operation.
     * @return {@code AppUser} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private AppUser createProvisionedUser(String email, String preferredUsername) {
        String baseUsername = sanitizeUsername(preferredUsername);
        if (baseUsername == null) {
            baseUsername = sanitizeUsername(email.contains("@") ? email.substring(0, email.indexOf('@')) : email);
        }
        if (baseUsername == null) {
            baseUsername = "sso-user";
        }
        String username = uniqueUsername(baseUsername);

        AppUser user = new AppUser();
        user.setUsername(username);
        user.setEmail(email);
        user.setPassword(passwordEncoder.encode("sso:" + UUID.randomUUID()));
        user.setRole(defaultRole);
        user.setActive(true);
        user.setMustResetPassword(false);
        user.setMfaRequired(false);
        user.setPermissions(rolePermissionService.defaultsForRole(defaultRole));
        user.setLanguagePreference("en");
        AppUser saved = appUserRepo.save(user);

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("email", email);
        metadata.put("username", username);
        metadata.put("role", defaultRole.name());
        metadata.put("autoProvision", true);
        auditEventService.record("SSO_USER_PROVISION", "USER", saved.getId(), null, null, metadata);
        return saved;
    }

    /**
     * Executes the recordSsoLogin operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param provider Parameter of type {@code String} used by this operation.
     * @param attributes Parameter of type {@code Map<String, Object>} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void recordSsoLogin(AppUser user, String provider, Map<String, Object> attributes) {
        UserAuditLog log = new UserAuditLog();
        log.setActorUsername(user.getUsername());
        log.setTargetUsername(user.getUsername());
        log.setAction("LOGIN_SSO");
        log.setDetails("provider=" + normalize(provider));
        userAuditLogRepo.save(log);

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("provider", normalize(provider));
        metadata.put("email", user.getEmail());
        metadata.put("subject", firstNonBlank(
                attributeValue(attributes, "sub"),
                attributeValue(attributes, "id")
        ));
        auditEventService.record("LOGIN_SSO", "AUTH", user.getId(), null, null, metadata);
    }

    /**
     * Executes the resolveEmail operation.
     *
     * @param attributes Parameter of type {@code Map<String, Object>} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String resolveEmail(Map<String, Object> attributes) {
        return normalize(firstNonBlank(
                attributeValue(attributes, emailClaim),
                attributeValue(attributes, "email"),
                attributeValue(attributes, "upn"),
                attributeValue(attributes, "preferred_username")
        ));
    }

    /**
     * Executes the attributeValue operation.
     *
     * @param attributes Parameter of type {@code Map<String, Object>} used by this operation.
     * @param key Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String attributeValue(Map<String, Object> attributes, String key) {
        if (attributes == null || key == null || key.isBlank()) {
            return null;
        }
        Object value = attributes.get(key);
        return value == null ? null : String.valueOf(value);
    }

    /**
     * Executes the firstNonBlank operation.
     *
     * @param values Parameter of type {@code String...} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String firstNonBlank(String... values) {
        if (values == null) {
            return null;
        }
        for (String value : values) {
            String normalized = normalize(value);
            if (normalized != null) {
                return normalized;
            }
        }
        return null;
    }

    /**
     * Executes the normalize operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalize(String value) {
        if (value == null) {
            return null;
        }
        String normalized = value.trim();
        return normalized.isEmpty() ? null : normalized;
    }

    /**
     * Executes the sanitizeUsername operation.
     *
     * @param candidate Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String sanitizeUsername(String candidate) {
        String normalized = normalize(candidate);
        if (normalized == null) {
            return null;
        }
        String sanitized = normalized.toLowerCase()
                .replaceAll("[^a-z0-9._-]", "-")
                .replaceAll("-{2,}", "-")
                .replaceAll("^[-.]+|[-.]+$", "");
        if (sanitized.isBlank()) {
            return null;
        }
        return sanitized.length() <= 40 ? sanitized : sanitized.substring(0, 40);
    }

    /**
     * Executes the uniqueUsername operation.
     *
     * @param base Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String uniqueUsername(String base) {
        if (!appUserRepo.existsByUsernameIgnoreCase(base)) {
            return base;
        }
        int suffix = 1;
        String candidate;
        do {
            candidate = base + "-" + suffix++;
        } while (appUserRepo.existsByUsernameIgnoreCase(candidate));
        return candidate;
    }

    /**
     * Executes the buildAuthorities operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @return {@code List<GrantedAuthority>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<GrantedAuthority> buildAuthorities(AppUser user) {
        List<GrantedAuthority> authorities = new ArrayList<>();
        authorities.add(new SimpleGrantedAuthority("ROLE_" + user.getRole().name()));

        if (user.getRole() != null) {
            if (user.getRole() == UserRole.SUPER_ADMIN) {
                authorities.add(new SimpleGrantedAuthority("ROLE_ADMIN"));
                authorities.add(new SimpleGrantedAuthority("ROLE_SUPER_ADMIN"));
            } else if (user.getRole() == UserRole.ADMIN) {
                authorities.add(new SimpleGrantedAuthority("ROLE_SUPER_ADMIN"));
            }

            if (user.getRole() == UserRole.BRANCH_MANAGER) {
                authorities.add(new SimpleGrantedAuthority("ROLE_MANAGER"));
                authorities.add(new SimpleGrantedAuthority("ROLE_BRANCH_MANAGER"));
            } else if (user.getRole() == UserRole.MANAGER) {
                authorities.add(new SimpleGrantedAuthority("ROLE_BRANCH_MANAGER"));
            }
        }

        if (user.getPermissions() != null) {
            for (Permission permission : user.getPermissions()) {
                authorities.add(new SimpleGrantedAuthority("PERM_" + permission.name()));
            }
        }
        return authorities;
    }
}
