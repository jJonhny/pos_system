package com.example.pos_system.service;

import com.example.pos_system.entity.AppUser;
import com.example.pos_system.entity.UserAuditLog;
import com.example.pos_system.repository.AppUserRepo;
import com.example.pos_system.repository.UserAuditLogRepo;
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

    public LoginSecurityService(AppUserRepo appUserRepo,
                                UserAuditLogRepo userAuditLogRepo,
                                AuditEventService auditEventService) {
        this.appUserRepo = appUserRepo;
        this.userAuditLogRepo = userAuditLogRepo;
        this.auditEventService = auditEventService;
    }

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

    public boolean isCurrentlyLocked(AppUser user) {
        return isCurrentlyLocked(user, LocalDateTime.now());
    }

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

    private boolean isCurrentlyLocked(AppUser user, LocalDateTime now) {
        if (user == null || user.getLockedUntil() == null) {
            return false;
        }
        return user.getLockedUntil().isAfter(now);
    }

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

    private int safeAttempts(AppUser user) {
        if (user == null || user.getFailedLoginAttempts() == null) {
            return 0;
        }
        return Math.max(0, user.getFailedLoginAttempts());
    }

    private int maxAllowedAttempts() {
        return Math.max(1, maxFailedAttempts);
    }

    private int lockMinutes() {
        return Math.max(1, lockDurationMinutes);
    }

    private String normalize(String raw) {
        if (raw == null) {
            return null;
        }
        String normalized = raw.trim();
        return normalized.isEmpty() ? null : normalized;
    }

    public record FailureOutcome(String reason, LocalDateTime lockedUntil) {
        private static FailureOutcome badCredentials() {
            return new FailureOutcome(REASON_BAD_CREDENTIALS, null);
        }

        private static FailureOutcome locked(LocalDateTime lockedUntil) {
            return new FailureOutcome(REASON_LOCKED, lockedUntil);
        }

        private static FailureOutcome disabled() {
            return new FailureOutcome(REASON_DISABLED, null);
        }
    }
}
