package com.example.pos_system.config;

import com.example.pos_system.entity.AppUser;
import com.example.pos_system.entity.UserAuditLog;
import com.example.pos_system.repository.AppUserRepo;
import com.example.pos_system.repository.UserAuditLogRepo;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.SavedRequestAwareAuthenticationSuccessHandler;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.time.LocalDateTime;

@Component
public class LoginSuccessHandler extends SavedRequestAwareAuthenticationSuccessHandler {
    private final AppUserRepo appUserRepo;
    private final UserAuditLogRepo auditLogRepo;

    public LoginSuccessHandler(AppUserRepo appUserRepo, UserAuditLogRepo auditLogRepo) {
        this.appUserRepo = appUserRepo;
        this.auditLogRepo = auditLogRepo;
        setDefaultTargetUrl("/");
    }

    @Override
    @Transactional
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
                                        Authentication authentication) throws IOException, ServletException {
        String username = authentication.getName();
        AppUser user = appUserRepo.findByUsername(username).orElse(null);
        if (user != null) {
            user.setLastLoginAt(LocalDateTime.now());
            appUserRepo.save(user);

            UserAuditLog log = new UserAuditLog();
            log.setActorUsername(username);
            log.setTargetUsername(username);
            log.setAction("LOGIN");
            log.setDetails("User logged in.");
            auditLogRepo.save(log);

            if (Boolean.TRUE.equals(user.getMustResetPassword())) {
                clearAuthenticationAttributes(request);
                response.sendRedirect("/users/password?force=1");
                return;
            }
        }
        super.onAuthenticationSuccess(request, response, authentication);
    }
}
