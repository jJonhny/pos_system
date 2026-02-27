package com.example.pos_system.service;

import com.example.pos_system.entity.AuditEvent;
import com.example.pos_system.repository.AppUserRepo;
import com.example.pos_system.repository.AuditEventRepo;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.time.LocalDateTime;

@Service
public class AuditEventService {
    private final AuditEventRepo auditEventRepo;
    private final AppUserRepo appUserRepo;
    private final ObjectMapper objectMapper = new ObjectMapper().findAndRegisterModules();

    /**
     * Executes the AuditEventService operation.
     * <p>Return value: A fully initialized AuditEventService instance.</p>
     *
     * @param auditEventRepo Parameter of type {@code AuditEventRepo} used by this operation.
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public AuditEventService(AuditEventRepo auditEventRepo, AppUserRepo appUserRepo) {
        this.auditEventRepo = auditEventRepo;
        this.appUserRepo = appUserRepo;
    }

    @Transactional
    public AuditEvent record(String actionType,
                             String targetType,
                             Object targetId,
                             Object beforeState,
                             Object afterState,
                             Object metadata) {
        Actor actor = resolveActor();
        HttpServletRequest request = currentRequest();
        AuditEvent event = AuditEvent.of(
                LocalDateTime.now(),
                actor.userId(),
                actor.username(),
                sanitize(actionType),
                sanitize(targetType),
                targetId == null ? null : String.valueOf(targetId),
                toJson(beforeState),
                toJson(afterState),
                toJson(metadata),
                extractIpAddress(request),
                extractTerminalId(request)
        );
        return auditEventRepo.save(event);
    }

    /**
     * Executes the resolveActor operation.
     *
     * @return {@code Actor} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Actor resolveActor() {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth == null || !auth.isAuthenticated()) {
            return new Actor(null, null);
        }
        String username = auth.getName();
        if (username == null || username.isBlank() || "anonymousUser".equalsIgnoreCase(username)) {
            return new Actor(null, null);
        }
        Long userId = appUserRepo.findByUsername(username).map(u -> u.getId()).orElse(null);
        return new Actor(userId, username);
    }

    /**
     * Executes the currentRequest operation.
     *
     * @return {@code HttpServletRequest} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private HttpServletRequest currentRequest() {
        var attributes = RequestContextHolder.getRequestAttributes();
        if (attributes instanceof ServletRequestAttributes servletAttributes) {
            return servletAttributes.getRequest();
        }
        return null;
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
        if (request == null) return null;
        String forwarded = sanitize(request.getHeader("X-Forwarded-For"));
        if (forwarded != null) {
            int comma = forwarded.indexOf(',');
            return comma >= 0 ? forwarded.substring(0, comma).trim() : forwarded;
        }
        String realIp = sanitize(request.getHeader("X-Real-IP"));
        if (realIp != null) return realIp;
        return sanitize(request.getRemoteAddr());
    }

    /**
     * Executes the extractTerminalId operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String extractTerminalId(HttpServletRequest request) {
        if (request == null) return null;
        String terminal = sanitize(request.getHeader("X-Terminal-Id"));
        if (terminal != null) return terminal;
        return sanitize(request.getHeader("X-POS-Terminal"));
    }

    /**
     * Executes the toJson operation.
     *
     * @param value Parameter of type {@code Object} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String toJson(Object value) {
        if (value == null) return null;
        try {
            return objectMapper.writeValueAsString(value);
        } catch (JsonProcessingException ex) {
            throw new IllegalStateException("Failed to serialize audit payload.", ex);
        }
    }

    /**
     * Executes the sanitize operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String sanitize(String value) {
        if (value == null) return null;
        String cleaned = value.trim();
        return cleaned.isEmpty() ? null : cleaned;
    }

    private record Actor(Long userId, String username) {}
}
