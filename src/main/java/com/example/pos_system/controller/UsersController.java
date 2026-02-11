package com.example.pos_system.controller;

import com.example.pos_system.entity.AppUser;
import com.example.pos_system.entity.Permission;
import com.example.pos_system.entity.UserAuditLog;
import com.example.pos_system.entity.UserRole;
import com.example.pos_system.repository.AppUserRepo;
import com.example.pos_system.repository.UserAuditLogRepo;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.core.Authentication;
import org.springframework.security.crypto.password.PasswordEncoder;
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

@Controller
@RequestMapping("/users")
public class UsersController {
    private static final int PAGE_SIZE = 20;
    private final AppUserRepo appUserRepo;
    private final PasswordEncoder passwordEncoder;
    private final UserAuditLogRepo auditLogRepo;

    public UsersController(AppUserRepo appUserRepo, PasswordEncoder passwordEncoder, UserAuditLogRepo auditLogRepo) {
        this.appUserRepo = appUserRepo;
        this.passwordEncoder = passwordEncoder;
        this.auditLogRepo = auditLogRepo;
    }

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
        model.addAttribute("auditLogs", auditLogRepo.findTop50ByOrderByCreatedAtDesc());
        return "users/list";
    }

    @PostMapping
    public String createUser(@RequestParam String username,
                             @RequestParam String password,
                             @RequestParam UserRole role,
                             @RequestParam(required = false) Boolean active,
                             @RequestParam(required = false) Boolean mustResetPassword,
                             @RequestParam(required = false) Boolean mfaRequired,
                             @RequestParam(required = false) List<Permission> permissions,
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

        AppUser user = new AppUser();
        user.setUsername(normalized);
        user.setPassword(passwordEncoder.encode(password));
        user.setRole(role);
        user.setActive(active == null || active);
        user.setMustResetPassword(mustResetPassword == null || mustResetPassword);
        user.setMfaRequired(mfaRequired != null && mfaRequired);
        if (permissions != null && !permissions.isEmpty()) {
            user.setPermissions(new HashSet<>(permissions));
        }
        appUserRepo.save(user);

        redirectAttributes.addFlashAttribute("success", "User created.");
        return "redirect:/users";
    }

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
        user.setRole(role);
        appUserRepo.save(user);
        logAction(authentication, user, "ROLE_UPDATE", "Role set to " + role.name());
        redirectAttributes.addFlashAttribute("success", "Role updated.");
        return "redirect:/users";
    }

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
        user.setPassword(passwordEncoder.encode(password));
        if (temporary == null || temporary) {
            user.setMustResetPassword(true);
        }
        appUserRepo.save(user);
        logAction(authentication, user, "PASSWORD_RESET", "Password reset by admin.");
        redirectAttributes.addFlashAttribute("success", "Password updated.");
        return "redirect:/users";
    }

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
        user.setActive(active);
        appUserRepo.save(user);
        logAction(authentication, user, active ? "ACTIVATE" : "DEACTIVATE", "Status set to " + (active ? "active" : "inactive"));
        redirectAttributes.addFlashAttribute("success", "Status updated.");
        return "redirect:/users";
    }

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
        user.setPermissions(newPerms);
        appUserRepo.save(user);
        logAction(authentication, user, "PERMISSIONS_UPDATE", "Permissions updated: " + newPerms);
        redirectAttributes.addFlashAttribute("success", "Permissions updated.");
        return "redirect:/users";
    }

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
        user.setMfaRequired(required);
        appUserRepo.save(user);
        logAction(authentication, user, "MFA_REQUIRED", "MFA required set to " + required);
        redirectAttributes.addFlashAttribute("success", "MFA requirement updated.");
        return "redirect:/users";
    }

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

        int updated = 0;
        for (AppUser user : users) {
            if (isSelf(authentication, user)) {
                continue;
            }
            switch (action) {
                case "activate" -> {
                    user.setActive(true);
                    updated++;
                    logAction(authentication, user, "ACTIVATE", "Bulk activate.");
                }
                case "deactivate" -> {
                    user.setActive(false);
                    updated++;
                    logAction(authentication, user, "DEACTIVATE", "Bulk deactivate.");
                }
                case "role" -> {
                    if (role == null) break;
                    if (user.getRole() == UserRole.ADMIN && role != UserRole.ADMIN
                            && appUserRepo.countByRole(UserRole.ADMIN) <= 1) {
                        break;
                    }
                    user.setRole(role);
                    updated++;
                    logAction(authentication, user, "ROLE_UPDATE", "Bulk role set to " + role.name());
                }
                case "add-perm" -> {
                    if (permission == null) break;
                    Set<Permission> perms = user.getPermissions() == null ? new HashSet<>() : new HashSet<>(user.getPermissions());
                    perms.add(permission);
                    user.setPermissions(perms);
                    updated++;
                    logAction(authentication, user, "PERMISSIONS_ADD", "Added permission " + permission.name());
                }
                case "remove-perm" -> {
                    if (permission == null) break;
                    Set<Permission> perms = user.getPermissions() == null ? new HashSet<>() : new HashSet<>(user.getPermissions());
                    perms.remove(permission);
                    user.setPermissions(perms);
                    updated++;
                    logAction(authentication, user, "PERMISSIONS_REMOVE", "Removed permission " + permission.name());
                }
                case "reset-password" -> {
                    if (password == null || password.isBlank()) break;
                    user.setPassword(passwordEncoder.encode(password));
                    user.setMustResetPassword(true);
                    updated++;
                    logAction(authentication, user, "PASSWORD_RESET", "Bulk password reset.");
                }
                case "require-mfa" -> {
                    user.setMfaRequired(true);
                    updated++;
                    logAction(authentication, user, "MFA_REQUIRED", "Bulk require MFA.");
                }
                case "clear-mfa" -> {
                    user.setMfaRequired(false);
                    updated++;
                    logAction(authentication, user, "MFA_REQUIRED", "Bulk clear MFA requirement.");
                }
                default -> {
                }
            }
        }
        appUserRepo.saveAll(users);
        redirectAttributes.addFlashAttribute("success", "Bulk action applied to " + updated + " users.");
        return "redirect:/users";
    }

    @GetMapping("/password")
    public String passwordForm(Model model, Authentication authentication) {
        model.addAttribute("forceReset", authentication != null);
        return "users/password";
    }

    @PostMapping("/password")
    public String updateOwnPassword(@RequestParam String password,
                                    Authentication authentication,
                                    RedirectAttributes redirectAttributes) {
        if (authentication == null) {
            redirectAttributes.addFlashAttribute("error", "Not authenticated.");
            return "redirect:/login";
        }
        if (password == null || password.isBlank()) {
            redirectAttributes.addFlashAttribute("error", "Password is required.");
            return "redirect:/users/password";
        }
        AppUser user = appUserRepo.findByUsername(authentication.getName()).orElse(null);
        if (user == null) {
            redirectAttributes.addFlashAttribute("error", "User not found.");
            return "redirect:/login";
        }
        user.setPassword(passwordEncoder.encode(password));
        user.setMustResetPassword(false);
        appUserRepo.save(user);
        logAction(authentication, user, "PASSWORD_CHANGE", "User updated their password.");
        redirectAttributes.addFlashAttribute("success", "Password updated.");
        return "redirect:/";
    }

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

    private boolean isSelf(Authentication authentication, AppUser user) {
        if (authentication == null || user == null) return false;
        String currentUsername = authentication.getName();
        return currentUsername != null && currentUsername.equalsIgnoreCase(user.getUsername());
    }

    private void logAction(Authentication authentication, AppUser target, String action, String details) {
        UserAuditLog log = new UserAuditLog();
        log.setActorUsername(authentication == null ? "system" : authentication.getName());
        log.setTargetUsername(target == null ? null : target.getUsername());
        log.setAction(action);
        log.setDetails(details);
        log.setCreatedAt(LocalDateTime.now());
        auditLogRepo.save(log);
    }
}
