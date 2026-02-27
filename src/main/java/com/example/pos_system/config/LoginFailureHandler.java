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

    /**
     * Executes the LoginFailureHandler operation.
     * <p>Return value: A fully initialized LoginFailureHandler instance.</p>
     *
     * @param loginSecurityService Parameter of type {@code LoginSecurityService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public LoginFailureHandler(LoginSecurityService loginSecurityService) {
        this.loginSecurityService = loginSecurityService;
    }

    /**
     * Executes the onAuthenticationFailure operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param exception Parameter of type {@code AuthenticationException} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * @throws ServletException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the onAuthenticationFailure operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param exception Parameter of type {@code AuthenticationException} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * @throws ServletException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the onAuthenticationFailure operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param exception Parameter of type {@code AuthenticationException} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * @throws ServletException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Override
    public void onAuthenticationFailure(HttpServletRequest request,
                                        HttpServletResponse response,
                                        AuthenticationException exception) throws IOException, ServletException {
        LoginSecurityService.FailureOutcome outcome =
                loginSecurityService.registerFailure(request.getParameter("username"), exception, request);

        String redirectUrl = buildRedirectUrl(outcome);
        getRedirectStrategy().sendRedirect(request, response, redirectUrl);
    }

    /**
     * Executes the buildRedirectUrl operation.
     *
     * @param outcome Parameter of type {@code LoginSecurityService.FailureOutcome} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String buildRedirectUrl(LoginSecurityService.FailureOutcome outcome) {
        StringBuilder url = new StringBuilder("/login?error=1&reason=");
        url.append(encode(outcome.reason()));

        if ("locked".equals(outcome.reason()) && outcome.lockedUntil() != null) {
            long remainingMinutes = remainingLockMinutes(outcome.lockedUntil());
            url.append("&lockedMinutes=").append(remainingMinutes);
        }
        return url.toString();
    }

    /**
     * Executes the remainingLockMinutes operation.
     *
     * @param lockedUntil Parameter of type {@code LocalDateTime} used by this operation.
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private long remainingLockMinutes(LocalDateTime lockedUntil) {
        long minutes = Duration.between(LocalDateTime.now(), lockedUntil).toMinutes();
        return Math.max(1, minutes);
    }

    /**
     * Executes the encode operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String encode(String value) {
        return URLEncoder.encode(value == null ? "" : value, StandardCharsets.UTF_8);
    }
}
