package com.devcore.pos_system.controller;

import com.devcore.pos_system.entity.AppUser;
import com.devcore.pos_system.entity.Permission;
import com.devcore.pos_system.entity.UserRole;
import com.devcore.pos_system.repository.AppUserRepo;
import com.devcore.pos_system.repository.UserAuditLogRepo;
import com.devcore.pos_system.service.UserAdminService;
import jakarta.servlet.http.HttpSession;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

@Controller
@RequestMapping("/users")
public class UsersController {
    private static final int PAGE_SIZE = 20;
    private static final String PASSWORD_CAPTCHA_SESSION_KEY = "users.password.captcha.code";
    private static final String PASSWORD_CAPTCHA_EXPIRY_SESSION_KEY = "users.password.captcha.expires-at";
    private static final int PASSWORD_CAPTCHA_TTL_MINUTES = 5;
    private final AppUserRepo appUserRepo;
    private final UserAuditLogRepo auditLogRepo;
    private final UserAdminService userAdminService;

    /**
     * Executes the UsersController operation.
     * <p>Return value: A fully initialized UsersController instance.</p>
     *
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * @param auditLogRepo Parameter of type {@code UserAuditLogRepo} used by this operation.
     * @param userAdminService Parameter of type {@code UserAdminService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public UsersController(AppUserRepo appUserRepo, UserAuditLogRepo auditLogRepo, UserAdminService userAdminService) {
        this.appUserRepo = appUserRepo;
        this.auditLogRepo = auditLogRepo;
        this.userAdminService = userAdminService;
    }

    /**
     * Executes the listUsers operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param page Parameter of type {@code int} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the listUsers operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param page Parameter of type {@code int} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the listUsers operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param page Parameter of type {@code int} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping
    public String listUsers(@RequestParam(required = false) String q,
                            @RequestParam(required = false) UserRole role,
                            @RequestParam(required = false) Boolean active,
                            @RequestParam(defaultValue = "0") int page,
                            Model model) {
        int pageNum = Math.max(0, page);
        Specification<AppUser> spec = buildSpecification(q, role, active);
        Page<AppUser> pageData = appUserRepo.findAll(spec, PageRequest.of(pageNum, PAGE_SIZE, Sort.by(Sort.Direction.ASC, "username")));
        List<AppUser> users = pageData.getContent();

        List<AppUser> filteredAll = appUserRepo.findAll(spec);
        long adminCount = filteredAll.stream().filter(u -> u.getRole() == UserRole.ADMIN).count();
        long managerCount = filteredAll.stream().filter(u -> u.getRole() == UserRole.MANAGER).count();
        long cashierCount = filteredAll.stream().filter(u -> u.getRole() == UserRole.CASHIER).count();
        Set<Long> lockedUserIds = users.stream()
                .filter(this::isAccountLocked)
                .map(AppUser::getId)
                .collect(Collectors.toSet());

        model.addAttribute("users", users);
        model.addAttribute("roles", UserRole.values());
        model.addAttribute("permissions", Permission.values());
        model.addAttribute("totalUsers", pageData.getTotalElements());
        model.addAttribute("adminCount", adminCount);
        model.addAttribute("managerCount", managerCount);
        model.addAttribute("cashierCount", cashierCount);
        model.addAttribute("page", pageData.getNumber());
        model.addAttribute("totalPages", Math.max(1, pageData.getTotalPages()));
        model.addAttribute("hasNext", pageData.hasNext());
        model.addAttribute("hasPrev", pageData.hasPrevious());
        model.addAttribute("nextPage", pageData.getNumber() + 1);
        model.addAttribute("prevPage", Math.max(0, pageData.getNumber() - 1));
        model.addAttribute("q", q);
        model.addAttribute("role", role);
        model.addAttribute("active", active);
        model.addAttribute("lockedUserIds", lockedUserIds);
        model.addAttribute("auditLogs", auditLogRepo.findTop50ByOrderByCreatedAtDesc());
        return "users/list";
    }

    /**
     * Executes the createUser operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param mustResetPassword Parameter of type {@code Boolean} used by this operation.
     * @param mfaRequired Parameter of type {@code Boolean} used by this operation.
     * @param permissions Parameter of type {@code List<Permission>} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createUser operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param mustResetPassword Parameter of type {@code Boolean} used by this operation.
     * @param mfaRequired Parameter of type {@code Boolean} used by this operation.
     * @param permissions Parameter of type {@code List<Permission>} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createUser operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param mustResetPassword Parameter of type {@code Boolean} used by this operation.
     * @param mfaRequired Parameter of type {@code Boolean} used by this operation.
     * @param permissions Parameter of type {@code List<Permission>} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping
    public String createUser(@RequestParam String username,
                             @RequestParam String password,
                             @RequestParam UserRole role,
                             @RequestParam(required = false) Boolean active,
                             @RequestParam(required = false) Boolean mustResetPassword,
                             @RequestParam(required = false) Boolean mfaRequired,
                             @RequestParam(required = false) List<Permission> permissions,
                             Authentication authentication,
                             RedirectAttributes redirectAttributes) {
        String normalized = username == null ? "" : username.trim();
        if (normalized.isEmpty()) {
            redirectAttributes.addFlashAttribute("error", "Username is required.");
            return "redirect:/users";
        }
        if (password == null || password.isBlank()) {
            redirectAttributes.addFlashAttribute("error", "Password is required.");
            return "redirect:/users";
        }
        if (appUserRepo.existsByUsernameIgnoreCase(normalized)) {
            redirectAttributes.addFlashAttribute("error", "Username already exists.");
            return "redirect:/users";
        }

        Set<Permission> permissionSet = permissions == null ? new HashSet<>() : new HashSet<>(permissions);
        userAdminService.createUser(
                normalized,
                password,
                role,
                active == null || active,
                mustResetPassword == null || mustResetPassword,
                mfaRequired != null && mfaRequired,
                permissionSet,
                authentication
        );

        redirectAttributes.addFlashAttribute("success", "User created.");
        return "redirect:/users";
    }

    /**
     * Executes the updateRole operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateRole operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateRole operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/{id}/role")
    public String updateRole(@PathVariable Long id,
                             @RequestParam UserRole role,
                             Authentication authentication,
                             RedirectAttributes redirectAttributes) {
        AppUser user = appUserRepo.findById(id).orElse(null);
        if (user == null) {
            redirectAttributes.addFlashAttribute("error", "User not found.");
            return "redirect:/users";
        }
        if (isSelf(authentication, user)) {
            redirectAttributes.addFlashAttribute("error", "You cannot change your own role.");
            return "redirect:/users";
        }
        if (user.getRole() == UserRole.ADMIN && role != UserRole.ADMIN && appUserRepo.countByRole(UserRole.ADMIN) <= 1) {
            redirectAttributes.addFlashAttribute("error", "You must keep at least one admin account.");
            return "redirect:/users";
        }
        userAdminService.updateRole(user, role, authentication, "Role set to " + role.name());
        redirectAttributes.addFlashAttribute("success", "Role updated.");
        return "redirect:/users";
    }

    /**
     * Executes the resetPassword operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param temporary Parameter of type {@code Boolean} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the resetPassword operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param temporary Parameter of type {@code Boolean} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the resetPassword operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param temporary Parameter of type {@code Boolean} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/{id}/password")
    public String resetPassword(@PathVariable Long id,
                                @RequestParam String password,
                                @RequestParam(required = false) Boolean temporary,
                                Authentication authentication,
                                RedirectAttributes redirectAttributes) {
        AppUser user = appUserRepo.findById(id).orElse(null);
        if (user == null) {
            redirectAttributes.addFlashAttribute("error", "User not found.");
            return "redirect:/users";
        }
        if (password == null || password.isBlank()) {
            redirectAttributes.addFlashAttribute("error", "Password is required.");
            return "redirect:/users";
        }
        userAdminService.resetPassword(user, password, temporary == null || temporary,
                authentication, "Password reset by admin.");
        redirectAttributes.addFlashAttribute("success", "Password updated.");
        return "redirect:/users";
    }

    /**
     * Executes the updateStatus operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateStatus operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateStatus operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/{id}/status")
    public String updateStatus(@PathVariable Long id,
                               @RequestParam Boolean active,
                               Authentication authentication,
                               RedirectAttributes redirectAttributes) {
        AppUser user = appUserRepo.findById(id).orElse(null);
        if (user == null) {
            redirectAttributes.addFlashAttribute("error", "User not found.");
            return "redirect:/users";
        }
        if (isSelf(authentication, user)) {
            redirectAttributes.addFlashAttribute("error", "You cannot deactivate your own account.");
            return "redirect:/users";
        }
        userAdminService.updateStatus(user, active, authentication,
                "Status set to " + (active ? "active" : "inactive"));
        redirectAttributes.addFlashAttribute("success", "Status updated.");
        return "redirect:/users";
    }

    /**
     * Executes the updatePermissions operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param permissions Parameter of type {@code List<Permission>} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updatePermissions operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param permissions Parameter of type {@code List<Permission>} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updatePermissions operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param permissions Parameter of type {@code List<Permission>} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/{id}/permissions")
    public String updatePermissions(@PathVariable Long id,
                                    @RequestParam(required = false) List<Permission> permissions,
                                    Authentication authentication,
                                    RedirectAttributes redirectAttributes) {
        AppUser user = appUserRepo.findById(id).orElse(null);
        if (user == null) {
            redirectAttributes.addFlashAttribute("error", "User not found.");
            return "redirect:/users";
        }
        if (isSelf(authentication, user)) {
            redirectAttributes.addFlashAttribute("error", "You cannot change your own permissions.");
            return "redirect:/users";
        }
        Set<Permission> newPerms = permissions == null ? new HashSet<>() : new HashSet<>(permissions);
        userAdminService.updatePermissions(user, newPerms, authentication, "Permissions updated: " + newPerms);
        redirectAttributes.addFlashAttribute("success", "Permissions updated.");
        return "redirect:/users";
    }

    /**
     * Executes the updateMfa operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param required Parameter of type {@code Boolean} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateMfa operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param required Parameter of type {@code Boolean} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateMfa operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param required Parameter of type {@code Boolean} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/{id}/mfa")
    public String updateMfa(@PathVariable Long id,
                            @RequestParam Boolean required,
                            Authentication authentication,
                            RedirectAttributes redirectAttributes) {
        AppUser user = appUserRepo.findById(id).orElse(null);
        if (user == null) {
            redirectAttributes.addFlashAttribute("error", "User not found.");
            return "redirect:/users";
        }
        if (isSelf(authentication, user)) {
            redirectAttributes.addFlashAttribute("error", "You cannot change your own MFA requirement.");
            return "redirect:/users";
        }
        userAdminService.updateMfa(user, required, authentication, "MFA required set to " + required);
        redirectAttributes.addFlashAttribute("success", "MFA requirement updated.");
        return "redirect:/users";
    }

    @PostMapping("/{id}/unlock")
    public String unlockAccount(@PathVariable Long id,
                                Authentication authentication,
                                RedirectAttributes redirectAttributes) {
        AppUser user = appUserRepo.findById(id).orElse(null);
        if (user == null) {
            redirectAttributes.addFlashAttribute("error", "User not found.");
            return "redirect:/users";
        }
        if (isSelf(authentication, user)) {
            redirectAttributes.addFlashAttribute("error", "Use your own security page to unlock your account.");
            return "redirect:/users";
        }
        userAdminService.unlockUser(user, authentication, "Account unlocked by admin.");
        redirectAttributes.addFlashAttribute("success", "Account unlocked.");
        return "redirect:/users";
    }

    @PostMapping("/{id}/otp/reset")
    public String resetOtpSetup(@PathVariable Long id,
                                Authentication authentication,
                                RedirectAttributes redirectAttributes) {
        AppUser user = appUserRepo.findById(id).orElse(null);
        if (user == null) {
            redirectAttributes.addFlashAttribute("error", "User not found.");
            return "redirect:/users";
        }
        if (isSelf(authentication, user)) {
            redirectAttributes.addFlashAttribute("error", "Use user-level security actions for your own account.");
            return "redirect:/users";
        }
        userAdminService.resetOtpSetup(user, authentication, "OTP setup reset by admin.");
        redirectAttributes.addFlashAttribute("success",
                "OTP setup reset. User must scan a new QR code on next login.");
        return "redirect:/users";
    }

    /**
     * Executes the bulkAction operation.
     *
     * @param ids Parameter of type {@code List<Long>} used by this operation.
     * @param action Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param permission Parameter of type {@code Permission} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the bulkAction operation.
     *
     * @param ids Parameter of type {@code List<Long>} used by this operation.
     * @param action Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param permission Parameter of type {@code Permission} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the bulkAction operation.
     *
     * @param ids Parameter of type {@code List<Long>} used by this operation.
     * @param action Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param permission Parameter of type {@code Permission} used by this operation.
     * @param password Parameter of type {@code String} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/bulk")
    public String bulkAction(@RequestParam(required = false) List<Long> ids,
                             @RequestParam String action,
                             @RequestParam(required = false) UserRole role,
                             @RequestParam(required = false) Permission permission,
                             @RequestParam(required = false) String password,
                             Authentication authentication,
                             RedirectAttributes redirectAttributes) {
        if (ids == null || ids.isEmpty()) {
            redirectAttributes.addFlashAttribute("error", "Select at least one user.");
            return "redirect:/users";
        }
        List<AppUser> users = appUserRepo.findAllById(ids);
        if (users.isEmpty()) {
            redirectAttributes.addFlashAttribute("error", "No users found.");
            return "redirect:/users";
        }

        String selfUsername = authentication == null ? null : authentication.getName();
        int updated = userAdminService.applyBulkAction(users, action, role, permission, password, authentication, selfUsername);
        redirectAttributes.addFlashAttribute("success", "Bulk action applied to " + updated + " users.");
        return "redirect:/users";
    }

    /**
     * Executes the passwordForm operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param session Parameter of type {@code HttpSession} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the passwordForm operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param session Parameter of type {@code HttpSession} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the passwordForm operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param session Parameter of type {@code HttpSession} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/password")
    public String passwordForm(Model model, Authentication authentication, HttpSession session) {
        AppUser currentUser = currentUser(authentication);
        model.addAttribute("forceReset", authentication != null);
        model.addAttribute("currentUser", currentUser);
        model.addAttribute("maskedContact", currentUser == null
                ? "Not available"
                : maskContact(currentUser.getEmail(), currentUser.getUsername()));
        model.addAttribute("captchaPending", hasValidCaptchaSession(session));
        model.addAttribute("isLocked", isAccountLocked(currentUser));
        return "users/password";
    }

    /**
     * Executes the sendPasswordCaptcha operation.
     *
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param session Parameter of type {@code HttpSession} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the sendPasswordCaptcha operation.
     *
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param session Parameter of type {@code HttpSession} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the sendPasswordCaptcha operation.
     *
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param session Parameter of type {@code HttpSession} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/password/captcha")
    public String sendPasswordCaptcha(Authentication authentication,
                                      HttpSession session,
                                      RedirectAttributes redirectAttributes) {
        if (authentication == null) {
            redirectAttributes.addFlashAttribute("error", "Not authenticated.");
            return "redirect:/login";
        }
        AppUser user = currentUser(authentication);
        if (user == null) {
            redirectAttributes.addFlashAttribute("error", "User not found.");
            return "redirect:/login";
        }

        String code = String.format("%06d", ThreadLocalRandom.current().nextInt(0, 1_000_000));
        LocalDateTime expiresAt = LocalDateTime.now().plusMinutes(PASSWORD_CAPTCHA_TTL_MINUTES);
        session.setAttribute(PASSWORD_CAPTCHA_SESSION_KEY, code);
        session.setAttribute(PASSWORD_CAPTCHA_EXPIRY_SESSION_KEY, expiresAt);
        redirectAttributes.addFlashAttribute("info",
                "Verification code sent to " + maskContact(user.getEmail(), user.getUsername())
                        + ". Demo code: " + code + ". Expires in " + PASSWORD_CAPTCHA_TTL_MINUTES + " minutes.");
        return "redirect:/users/password";
    }

    /**
     * Executes the updateOwnMfa operation.
     *
     * @param enabled Parameter of type {@code Boolean} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateOwnMfa operation.
     *
     * @param enabled Parameter of type {@code Boolean} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateOwnMfa operation.
     *
     * @param enabled Parameter of type {@code Boolean} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/password/mfa")
    public String updateOwnMfa(@RequestParam(defaultValue = "false") Boolean enabled,
                               Authentication authentication,
                               RedirectAttributes redirectAttributes) {
        if (authentication == null) {
            redirectAttributes.addFlashAttribute("error", "Not authenticated.");
            return "redirect:/login";
        }
        AppUser user = currentUser(authentication);
        if (user == null) {
            redirectAttributes.addFlashAttribute("error", "User not found.");
            return "redirect:/login";
        }
        boolean requireMfa = enabled != null && enabled;
        userAdminService.updateOwnMfa(user, requireMfa, authentication);
        redirectAttributes.addFlashAttribute("success", requireMfa
                ? "MFA requirement enabled."
                : "MFA requirement disabled.");
        return "redirect:/users/password";
    }

    /**
     * Executes the resetOwnSecurityCounters operation.
     *
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the resetOwnSecurityCounters operation.
     *
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the resetOwnSecurityCounters operation.
     *
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/password/unlock")
    public String resetOwnSecurityCounters(Authentication authentication,
                                           RedirectAttributes redirectAttributes) {
        if (authentication == null) {
            redirectAttributes.addFlashAttribute("error", "Not authenticated.");
            return "redirect:/login";
        }
        AppUser user = currentUser(authentication);
        if (user == null) {
            redirectAttributes.addFlashAttribute("error", "User not found.");
            return "redirect:/login";
        }
        userAdminService.clearOwnLoginSecurityState(user, authentication);
        redirectAttributes.addFlashAttribute("success", "Login protection counters reset.");
        return "redirect:/users/password";
    }

    /**
     * Executes the updateOwnPassword operation.
     *
     * @param password Parameter of type {@code String} used by this operation.
     * @param confirmPassword Parameter of type {@code String} used by this operation.
     * @param captcha Parameter of type {@code String} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param session Parameter of type {@code HttpSession} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateOwnPassword operation.
     *
     * @param password Parameter of type {@code String} used by this operation.
     * @param confirmPassword Parameter of type {@code String} used by this operation.
     * @param captcha Parameter of type {@code String} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param session Parameter of type {@code HttpSession} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateOwnPassword operation.
     *
     * @param password Parameter of type {@code String} used by this operation.
     * @param confirmPassword Parameter of type {@code String} used by this operation.
     * @param captcha Parameter of type {@code String} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param session Parameter of type {@code HttpSession} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/password")
    public String updateOwnPassword(@RequestParam String password,
                                    @RequestParam(required = false) String confirmPassword,
                                    @RequestParam(required = false) String captcha,
                                    Authentication authentication,
                                    HttpSession session,
                                    RedirectAttributes redirectAttributes) {
        if (authentication == null) {
            redirectAttributes.addFlashAttribute("error", "Not authenticated.");
            return "redirect:/login";
        }
        if (password == null || password.isBlank()) {
            redirectAttributes.addFlashAttribute("error", "Password is required.");
            return "redirect:/users/password";
        }
        if (password.length() < 8) {
            redirectAttributes.addFlashAttribute("error", "Password must be at least 8 characters.");
            return "redirect:/users/password";
        }
        if (confirmPassword == null || !password.equals(confirmPassword)) {
            redirectAttributes.addFlashAttribute("error", "Password confirmation does not match.");
            return "redirect:/users/password";
        }
        if (!isValidCaptcha(captcha, session)) {
            redirectAttributes.addFlashAttribute("error", "Invalid or expired captcha. Please request a new code.");
            return "redirect:/users/password";
        }
        AppUser user = currentUser(authentication);
        if (user == null) {
            redirectAttributes.addFlashAttribute("error", "User not found.");
            return "redirect:/login";
        }
        userAdminService.updateOwnPassword(user, password, authentication);
        clearPasswordCaptcha(session);
        redirectAttributes.addFlashAttribute("success", "Password updated.");
        return "redirect:/";
    }

    /**
     * Executes the buildSpecification operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @return {@code Specification<AppUser>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Specification<AppUser> buildSpecification(String q, UserRole role, Boolean active) {
        return (root, query, cb) -> {
            List<Predicate> predicates = new ArrayList<>();
            if (q != null && !q.isBlank()) {
                String like = "%" + q.trim().toLowerCase() + "%";
                predicates.add(cb.like(cb.lower(root.get("username")), like));
            }
            if (role != null) {
                predicates.add(cb.equal(root.get("role"), role));
            }
            if (active != null) {
                predicates.add(cb.equal(root.get("active"), active));
            }
            return cb.and(predicates.toArray(new Predicate[0]));
        };
    }

    /**
     * Executes the isSelf operation.
     *
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean isSelf(Authentication authentication, AppUser user) {
        if (authentication == null || user == null) return false;
        String currentUsername = authentication.getName();
        return currentUsername != null && currentUsername.equalsIgnoreCase(user.getUsername());
    }

    private boolean isAccountLocked(AppUser user) {
        return user != null
                && user.getLockedUntil() != null
                && user.getLockedUntil().isAfter(LocalDateTime.now());
    }

    /**
     * Executes the currentUser operation.
     *
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @return {@code AppUser} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private AppUser currentUser(Authentication authentication) {
        if (authentication == null || authentication.getName() == null || authentication.getName().isBlank()) {
            return null;
        }
        return appUserRepo.findByUsernameIgnoreCase(authentication.getName()).orElse(null);
    }

    /**
     * Executes the hasValidCaptchaSession operation.
     *
     * @param session Parameter of type {@code HttpSession} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean hasValidCaptchaSession(HttpSession session) {
        if (session == null) return false;
        Object code = session.getAttribute(PASSWORD_CAPTCHA_SESSION_KEY);
        Object expiry = session.getAttribute(PASSWORD_CAPTCHA_EXPIRY_SESSION_KEY);
        if (!(code instanceof String) || !(expiry instanceof LocalDateTime expiryTime)) {
            return false;
        }
        if (expiryTime.isBefore(LocalDateTime.now())) {
            clearPasswordCaptcha(session);
            return false;
        }
        return true;
    }

    /**
     * Executes the isValidCaptcha operation.
     *
     * @param submittedCaptcha Parameter of type {@code String} used by this operation.
     * @param session Parameter of type {@code HttpSession} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean isValidCaptcha(String submittedCaptcha, HttpSession session) {
        if (submittedCaptcha == null || submittedCaptcha.isBlank() || !hasValidCaptchaSession(session)) {
            return false;
        }
        Object storedCode = session.getAttribute(PASSWORD_CAPTCHA_SESSION_KEY);
        boolean matches = storedCode instanceof String code
                && code.equals(submittedCaptcha.trim());
        if (matches) {
            clearPasswordCaptcha(session);
        }
        return matches;
    }

    /**
     * Executes the clearPasswordCaptcha operation.
     *
     * @param session Parameter of type {@code HttpSession} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void clearPasswordCaptcha(HttpSession session) {
        if (session == null) return;
        session.removeAttribute(PASSWORD_CAPTCHA_SESSION_KEY);
        session.removeAttribute(PASSWORD_CAPTCHA_EXPIRY_SESSION_KEY);
    }

    /**
     * Executes the maskContact operation.
     *
     * @param email Parameter of type {@code String} used by this operation.
     * @param fallbackUsername Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String maskContact(String email, String fallbackUsername) {
        String source = email != null && !email.isBlank() ? email : fallbackUsername;
        if (source == null || source.isBlank()) {
            return "Not available";
        }
        int atIndex = source.indexOf('@');
        if (atIndex > 0) {
            String local = source.substring(0, atIndex);
            String domain = source.substring(atIndex + 1);
            return maskChunk(local, 2) + "@" + maskDomain(domain);
        }
        return maskChunk(source, 2);
    }

    /**
     * Executes the maskChunk operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @param keepPrefix Parameter of type {@code int} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String maskChunk(String value, int keepPrefix) {
        if (value == null || value.isBlank()) return "***";
        if (value.length() <= 1) return "*";
        int prefixLen = Math.min(keepPrefix, value.length() - 1);
        return value.substring(0, prefixLen) + "***" + value.charAt(value.length() - 1);
    }

    /**
     * Executes the maskDomain operation.
     *
     * @param domain Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String maskDomain(String domain) {
        if (domain == null || domain.isBlank()) return "***";
        int dotIndex = domain.lastIndexOf('.');
        if (dotIndex <= 0 || dotIndex >= domain.length() - 1) {
            return maskChunk(domain, 1);
        }
        String host = domain.substring(0, dotIndex);
        String tld = domain.substring(dotIndex);
        return maskChunk(host, 1) + tld;
    }
}
