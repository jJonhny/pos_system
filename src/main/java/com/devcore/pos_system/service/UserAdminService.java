package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.AppUser;
import com.devcore.pos_system.entity.Permission;
import com.devcore.pos_system.entity.UserAuditLog;
import com.devcore.pos_system.entity.UserRole;
import com.devcore.pos_system.repository.AppUserRepo;
import com.devcore.pos_system.repository.UserAuditLogRepo;
import org.springframework.security.core.Authentication;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Service
@Transactional
public class UserAdminService {
    private final AppUserRepo appUserRepo;
    private final PasswordEncoder passwordEncoder;
    private final UserAuditLogRepo auditLogRepo;
    private final AuditEventService auditEventService;
    private final RolePermissionService rolePermissionService;

    /**
     * Executes the UserAdminService operation.
     * <p>Return value: A fully initialized UserAdminService instance.</p>
     *
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * @param passwordEncoder Parameter of type {@code PasswordEncoder} used by this operation.
     * @param auditLogRepo Parameter of type {@code UserAuditLogRepo} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * @param rolePermissionService Parameter of type {@code RolePermissionService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public UserAdminService(AppUserRepo appUserRepo, PasswordEncoder passwordEncoder,
                            UserAuditLogRepo auditLogRepo, AuditEventService auditEventService,
                            RolePermissionService rolePermissionService) {
        this.appUserRepo = appUserRepo;
        this.passwordEncoder = passwordEncoder;
        this.auditLogRepo = auditLogRepo;
        this.auditEventService = auditEventService;
        this.rolePermissionService = rolePermissionService;
    }

    /**
     * Executes the createUser operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param active Parameter of type {@code boolean} used by this operation.
     * @param mustResetPassword Parameter of type {@code boolean} used by this operation.
     * @param mfaRequired Parameter of type {@code boolean} used by this operation.
     * @param permissions Parameter of type {@code Set<Permission>} used by this operation.
     * @param actor Parameter of type {@code Authentication} used by this operation.
     * @return {@code AppUser} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public AppUser createUser(String username,
                              String password,
                              UserRole role,
                              boolean active,
                              boolean mustResetPassword,
                              boolean mfaRequired,
                              Set<Permission> permissions,
                              Authentication actor) {
        AppUser user = new AppUser();
        user.setUsername(username);
        user.setPassword(passwordEncoder.encode(password));
        user.setRole(role);
        user.setActive(active);
        user.setMustResetPassword(mustResetPassword);
        user.setMfaRequired(mfaRequired);
        Set<Permission> effectivePermissions = (permissions == null || permissions.isEmpty())
                ? rolePermissionService.defaultsForRole(role)
                : permissions;
        user.setPermissions(effectivePermissions);
        AppUser saved = appUserRepo.save(user);
        recordAction(actor, saved, "USER_CREATE", "User created.");
        auditEventService.record("USER_CREATE", "USER", saved.getId(), null, userSnapshot(saved), null);
        return saved;
    }

    /**
     * Executes the updateRole operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param actor Parameter of type {@code Authentication} used by this operation.
     * @param details Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void updateRole(AppUser user, UserRole role, Authentication actor, String details) {
        Map<String, Object> before = userSnapshot(user);
        user.setRole(role);
        AppUser saved = appUserRepo.save(user);
        recordAction(actor, saved, "ROLE_UPDATE", details);
        auditEventService.record("ROLE_UPDATE", "USER", saved.getId(), before, userSnapshot(saved),
                Map.of("details", details));
    }

    /**
     * Executes the resetPassword operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param rawPassword Parameter of type {@code String} used by this operation.
     * @param temporary Parameter of type {@code boolean} used by this operation.
     * @param actor Parameter of type {@code Authentication} used by this operation.
     * @param details Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void resetPassword(AppUser user, String rawPassword, boolean temporary,
                              Authentication actor, String details) {
        Map<String, Object> before = userSnapshot(user);
        user.setPassword(passwordEncoder.encode(rawPassword));
        user.setFailedLoginAttempts(0);
        user.setLastFailedLoginAt(null);
        user.setLockedUntil(null);
        if (temporary) {
            user.setMustResetPassword(true);
        }
        AppUser saved = appUserRepo.save(user);
        recordAction(actor, saved, "PASSWORD_RESET", details);
        auditEventService.record("PASSWORD_RESET", "USER", saved.getId(), before, userSnapshot(saved),
                Map.of("temporary", temporary, "details", details));
    }

    /**
     * Executes the updateStatus operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param active Parameter of type {@code boolean} used by this operation.
     * @param actor Parameter of type {@code Authentication} used by this operation.
     * @param details Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void updateStatus(AppUser user, boolean active, Authentication actor, String details) {
        Map<String, Object> before = userSnapshot(user);
        user.setActive(active);
        AppUser saved = appUserRepo.save(user);
        recordAction(actor, saved, active ? "ACTIVATE" : "DEACTIVATE", details);
        auditEventService.record("USER_STATUS_UPDATE", "USER", saved.getId(), before, userSnapshot(saved),
                Map.of("active", active, "details", details));
    }

    /**
     * Executes the updatePermissions operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param permissions Parameter of type {@code Set<Permission>} used by this operation.
     * @param actor Parameter of type {@code Authentication} used by this operation.
     * @param details Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void updatePermissions(AppUser user, Set<Permission> permissions, Authentication actor, String details) {
        Map<String, Object> before = userSnapshot(user);
        user.setPermissions(permissions);
        AppUser saved = appUserRepo.save(user);
        recordAction(actor, saved, "PERMISSIONS_UPDATE", details);
        auditEventService.record("PERMISSIONS_UPDATE", "USER", saved.getId(), before, userSnapshot(saved),
                Map.of("details", details));
    }

    /**
     * Executes the updateMfa operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param required Parameter of type {@code boolean} used by this operation.
     * @param actor Parameter of type {@code Authentication} used by this operation.
     * @param details Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void updateMfa(AppUser user, boolean required, Authentication actor, String details) {
        Map<String, Object> before = userSnapshot(user);
        user.setMfaRequired(required);
        AppUser saved = appUserRepo.save(user);
        recordAction(actor, saved, "MFA_REQUIRED", details);
        auditEventService.record("MFA_UPDATE", "USER", saved.getId(), before, userSnapshot(saved),
                Map.of("required", required, "details", details));
    }

    public void unlockUser(AppUser user, Authentication actor, String details) {
        Map<String, Object> before = userSnapshot(user);
        user.setFailedLoginAttempts(0);
        user.setLastFailedLoginAt(null);
        user.setLockedUntil(null);
        AppUser saved = appUserRepo.save(user);
        recordAction(actor, saved, "ACCOUNT_UNLOCK", details);
        auditEventService.record("ACCOUNT_UNLOCK", "USER", saved.getId(), before, userSnapshot(saved),
                Map.of("details", details));
    }

    public void resetOtpSetup(AppUser user, Authentication actor, String details) {
        Map<String, Object> before = userSnapshot(user);
        user.setTotpSecret(null);
        user.setTotpEnabled(false);
        user.setLastTotpVerifiedAt(null);
        AppUser saved = appUserRepo.save(user);
        recordAction(actor, saved, "OTP_SETUP_RESET", details);
        auditEventService.record("OTP_SETUP_RESET", "USER", saved.getId(), before, userSnapshot(saved),
                Map.of("details", details));
    }

    /**
     * Executes the updateOwnPassword operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param actor Parameter of type {@code Authentication} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void updateOwnPassword(AppUser user, String password, Authentication actor) {
        Map<String, Object> before = userSnapshot(user);
        user.setPassword(passwordEncoder.encode(password));
        user.setMustResetPassword(false);
        user.setFailedLoginAttempts(0);
        user.setLastFailedLoginAt(null);
        user.setLockedUntil(null);
        AppUser saved = appUserRepo.save(user);
        recordAction(actor, saved, "PASSWORD_CHANGE", "User updated their password.");
        auditEventService.record("PASSWORD_CHANGE", "USER", saved.getId(), before, userSnapshot(saved), null);
    }

    /**
     * Executes the updateOwnMfa operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param required Parameter of type {@code boolean} used by this operation.
     * @param actor Parameter of type {@code Authentication} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void updateOwnMfa(AppUser user, boolean required, Authentication actor) {
        Map<String, Object> before = userSnapshot(user);
        user.setMfaRequired(required);
        AppUser saved = appUserRepo.save(user);
        recordAction(actor, saved, "MFA_SELF_UPDATE", "User changed own MFA requirement to " + required + ".");
        auditEventService.record("MFA_SELF_UPDATE", "USER", saved.getId(), before, userSnapshot(saved),
                Map.of("required", required));
    }

    /**
     * Executes the clearOwnLoginSecurityState operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @param actor Parameter of type {@code Authentication} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void clearOwnLoginSecurityState(AppUser user, Authentication actor) {
        Map<String, Object> before = userSnapshot(user);
        user.setFailedLoginAttempts(0);
        user.setLastFailedLoginAt(null);
        user.setLockedUntil(null);
        AppUser saved = appUserRepo.save(user);
        recordAction(actor, saved, "LOGIN_SECURITY_RESET", "User reset own login security counters.");
        auditEventService.record("LOGIN_SECURITY_RESET", "USER", saved.getId(), before, userSnapshot(saved), null);
    }

    /**
     * Executes the applyBulkAction operation.
     *
     * @param users Parameter of type {@code List<AppUser>} used by this operation.
     * @param action Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param permission Parameter of type {@code Permission} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param actor Parameter of type {@code Authentication} used by this operation.
     * @param selfUsername Parameter of type {@code String} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public int applyBulkAction(List<AppUser> users,
                               String action,
                               UserRole role,
                               Permission permission,
                               String password,
                               Authentication actor,
                               String selfUsername) {
        int updated = 0;
        for (AppUser user : users) {
            if (user == null) continue;
            if (selfUsername != null && selfUsername.equalsIgnoreCase(user.getUsername())) {
                continue;
            }
            switch (action) {
                case "activate" -> {
                    updateStatus(user, true, actor, "Bulk activate.");
                    updated++;
                }
                case "deactivate" -> {
                    updateStatus(user, false, actor, "Bulk deactivate.");
                    updated++;
                }
                case "role" -> {
                    if (role == null) break;
                    if (user.getRole() == UserRole.ADMIN && role != UserRole.ADMIN
                            && appUserRepo.countByRole(UserRole.ADMIN) <= 1) {
                        break;
                    }
                    updateRole(user, role, actor, "Bulk role set to " + role.name());
                    updated++;
                }
                case "add-perm" -> {
                    if (permission == null) break;
                    Set<Permission> perms = user.getPermissions() == null ? new java.util.HashSet<>() : new java.util.HashSet<>(user.getPermissions());
                    perms.add(permission);
                    updatePermissions(user, perms, actor, "Added permission " + permission.name());
                    updated++;
                }
                case "remove-perm" -> {
                    if (permission == null) break;
                    Set<Permission> perms = user.getPermissions() == null ? new java.util.HashSet<>() : new java.util.HashSet<>(user.getPermissions());
                    perms.remove(permission);
                    updatePermissions(user, perms, actor, "Removed permission " + permission.name());
                    updated++;
                }
                case "reset-password" -> {
                    if (password == null || password.isBlank()) break;
                    resetPassword(user, password, true, actor, "Bulk password reset.");
                    updated++;
                }
                case "require-mfa" -> {
                    updateMfa(user, true, actor, "Bulk require MFA.");
                    updated++;
                }
                case "clear-mfa" -> {
                    updateMfa(user, false, actor, "Bulk clear MFA requirement.");
                    updated++;
                }
                case "unlock" -> {
                    unlockUser(user, actor, "Bulk unlock account.");
                    updated++;
                }
                case "reset-otp" -> {
                    resetOtpSetup(user, actor, "Bulk reset OTP setup.");
                    updated++;
                }
                default -> {
                }
            }
        }
        return updated;
    }

    /**
     * Executes the recordAction operation.
     *
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param target Parameter of type {@code AppUser} used by this operation.
     * @param action Parameter of type {@code String} used by this operation.
     * @param details Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void recordAction(Authentication authentication, AppUser target, String action, String details) {
        UserAuditLog log = new UserAuditLog();
        log.setActorUsername(authentication == null ? "system" : authentication.getName());
        log.setTargetUsername(target == null ? null : target.getUsername());
        log.setAction(action);
        log.setDetails(details);
        log.setCreatedAt(LocalDateTime.now());
        auditLogRepo.save(log);
    }

    /**
     * Executes the userSnapshot operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @return {@code Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, Object> userSnapshot(AppUser user) {
        Map<String, Object> snapshot = new LinkedHashMap<>();
        snapshot.put("id", user.getId());
        snapshot.put("username", user.getUsername());
        snapshot.put("role", user.getRole() == null ? null : user.getRole().name());
        snapshot.put("active", user.getActive());
        snapshot.put("mustResetPassword", user.getMustResetPassword());
        snapshot.put("mfaRequired", user.getMfaRequired());
        snapshot.put("permissions", user.getPermissions() == null ? List.of() : user.getPermissions());
        snapshot.put("lastLoginAt", user.getLastLoginAt());
        snapshot.put("failedLoginAttempts", user.getFailedLoginAttempts());
        snapshot.put("lastFailedLoginAt", user.getLastFailedLoginAt());
        snapshot.put("lockedUntil", user.getLockedUntil());
        snapshot.put("totpEnabled", user.getTotpEnabled());
        snapshot.put("lastTotpVerifiedAt", user.getLastTotpVerifiedAt());
        return snapshot;
    }
}
