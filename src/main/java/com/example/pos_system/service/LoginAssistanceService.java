package com.example.pos_system.service;

import com.example.pos_system.repository.AppUserRepo;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.LinkedHashMap;
import java.util.Map;

@Service
@Transactional
public class LoginAssistanceService {
    private final AppUserRepo appUserRepo;
    private final AuditEventService auditEventService;

    public LoginAssistanceService(AppUserRepo appUserRepo, AuditEventService auditEventService) {
        this.appUserRepo = appUserRepo;
        this.auditEventService = auditEventService;
    }

    public void requestPasswordHelp(String rawUsername) {
        String username = normalize(rawUsername);
        boolean knownUser = username != null && appUserRepo.existsByUsernameIgnoreCase(username);

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("usernameProvided", username != null);
        metadata.put("knownUser", knownUser);
        if (username != null) {
            metadata.put("username", username);
        }

        auditEventService.record(
                "LOGIN_PASSWORD_HELP_REQUEST",
                "AUTH",
                username,
                null,
                null,
                metadata
        );
    }

    public void requestSsoSignIn() {
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("status", "not_configured");
        auditEventService.record(
                "LOGIN_SSO_REQUEST",
                "AUTH",
                null,
                null,
                null,
                metadata
        );
    }

    private String normalize(String raw) {
        if (raw == null) {
            return null;
        }
        String normalized = raw.trim();
        return normalized.isEmpty() ? null : normalized;
    }
}
