package com.devcore.pos_system.config;

import com.devcore.pos_system.entity.AppUser;
import com.devcore.pos_system.entity.UserAuditLog;
import com.devcore.pos_system.repository.AppUserRepo;
import com.devcore.pos_system.repository.UserAuditLogRepo;
import com.devcore.pos_system.service.LoginSecurityService;
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
    private final LoginSecurityService loginSecurityService;

    /**
     * Executes the LoginSuccessHandler operation.
     * <p>Return value: A fully initialized LoginSuccessHandler instance.</p>
     *
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * @param auditLogRepo Parameter of type {@code UserAuditLogRepo} used by this operation.
     * @param loginSecurityService Parameter of type {@code LoginSecurityService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public LoginSuccessHandler(AppUserRepo appUserRepo,
                               UserAuditLogRepo auditLogRepo,
                               LoginSecurityService loginSecurityService) {
        this.appUserRepo = appUserRepo;
        this.auditLogRepo = auditLogRepo;
        this.loginSecurityService = loginSecurityService;
        setDefaultTargetUrl("/");
    }

    /**
     * Executes the onAuthenticationSuccess operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * @throws ServletException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the onAuthenticationSuccess operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * @throws ServletException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the onAuthenticationSuccess operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * @throws ServletException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Override
    @Transactional
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
                                        Authentication authentication) throws IOException, ServletException {
        String username = authentication.getName();
        loginSecurityService.registerSuccess(username);
        AppUser user = appUserRepo.findByUsernameIgnoreCase(username).orElse(null);
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
