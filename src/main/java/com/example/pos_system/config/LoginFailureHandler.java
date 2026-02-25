package com.example.pos_system.config;

import com.example.pos_system.service.LoginSecurityService;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationFailureHandler;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.LocalDateTime;

@Component
public class LoginFailureHandler extends SimpleUrlAuthenticationFailureHandler {
    private final LoginSecurityService loginSecurityService;

    public LoginFailureHandler(LoginSecurityService loginSecurityService) {
        this.loginSecurityService = loginSecurityService;
    }

    @Override
    public void onAuthenticationFailure(HttpServletRequest request,
                                        HttpServletResponse response,
                                        AuthenticationException exception) throws IOException, ServletException {
        LoginSecurityService.FailureOutcome outcome =
                loginSecurityService.registerFailure(request.getParameter("username"), exception, request);

        String redirectUrl = buildRedirectUrl(outcome);
        getRedirectStrategy().sendRedirect(request, response, redirectUrl);
    }

    private String buildRedirectUrl(LoginSecurityService.FailureOutcome outcome) {
        StringBuilder url = new StringBuilder("/login?error=1&reason=");
        url.append(encode(outcome.reason()));

        if ("locked".equals(outcome.reason()) && outcome.lockedUntil() != null) {
            long remainingMinutes = remainingLockMinutes(outcome.lockedUntil());
            url.append("&lockedMinutes=").append(remainingMinutes);
        }
        return url.toString();
    }

    private long remainingLockMinutes(LocalDateTime lockedUntil) {
        long minutes = Duration.between(LocalDateTime.now(), lockedUntil).toMinutes();
        return Math.max(1, minutes);
    }

    private String encode(String value) {
        return URLEncoder.encode(value == null ? "" : value, StandardCharsets.UTF_8);
    }
}
