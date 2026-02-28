package com.devcore.pos_system.controller;

import com.devcore.pos_system.config.LoginSuccessHandler;
import com.devcore.pos_system.service.AppUserDetailsService;
import com.devcore.pos_system.service.AuthService;
import com.devcore.pos_system.service.LoginAssistanceService;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;
import org.springframework.security.web.context.SecurityContextRepository;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.server.ResponseStatusException;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

@Controller
public class LoginController {
    private static final String SESSION_OTP_CHALLENGE = "LOGIN_OTP_CHALLENGE";
    private static final Logger log = LoggerFactory.getLogger(LoginController.class);
    private static final SecurityContextRepository SECURITY_CONTEXT_REPOSITORY =
            new HttpSessionSecurityContextRepository();

    private final LoginAssistanceService loginAssistanceService;
    private final AuthService authService;
    private final AppUserDetailsService appUserDetailsService;
    private final LoginSuccessHandler loginSuccessHandler;

    public LoginController(LoginAssistanceService loginAssistanceService,
                           AuthService authService,
                           AppUserDetailsService appUserDetailsService,
                           LoginSuccessHandler loginSuccessHandler) {
        this.loginAssistanceService = loginAssistanceService;
        this.authService = authService;
        this.appUserDetailsService = appUserDetailsService;
        this.loginSuccessHandler = loginSuccessHandler;
    }

    @GetMapping("/login")
    public String login(Model model, HttpSession session) {
        model.addAttribute("ssoEnabled", loginAssistanceService.isSsoReady());

        OtpChallengeState otpChallenge = readOtpChallenge(session);
        model.addAttribute("otpPending", otpChallenge != null);
        if (otpChallenge != null) {
            model.addAttribute("otpFirstTimeSetup", otpChallenge.firstTimeSetup());
            model.addAttribute("otpQrDataUrl", otpChallenge.qrDataUrl());
            model.addAttribute("otpAuthUrl", otpChallenge.otpauthUrl());
        }
        return "login";
    }

    @PostMapping("/login/password")
    public String passwordStep(@RequestParam(value = "username", required = false) String identifier,
                               @RequestParam(value = "password", required = false) String password,
                               HttpServletRequest request,
                               HttpSession session) {
        clearOtpChallenge(session);

        AuthService.LoginResult result;
        try {
            result = authService.loginWithIdentifier(identifier, password, request);
        } catch (ResponseStatusException ex) {
            HttpStatus status = HttpStatus.resolve(ex.getStatusCode().value());
            if (status == HttpStatus.SERVICE_UNAVAILABLE) {
                return "redirect:/login?error=1&reason=otp-service-unavailable";
            }
            return "redirect:/login?error=1&reason=bad-credentials";
        }

        if (result.status() == AuthService.LoginStatus.LOCKED) {
            return "redirect:/login?error=1&reason=locked&lockedMinutes=" + safeLockedMinutes(result.lockedMinutes());
        }
        if (result.status() == AuthService.LoginStatus.DISABLED) {
            return "redirect:/login?error=1&reason=disabled";
        }
        if (result.status() == AuthService.LoginStatus.INVALID_CREDENTIALS) {
            return "redirect:/login?error=1&reason=bad-credentials";
        }
        if (result.status() == AuthService.LoginStatus.OTP_REQUIRED) {
            storeOtpChallenge(session, normalizeIdentifier(identifier), result);
            return "redirect:/login?otp=1";
        }

        return "redirect:/login?error=1&reason=bad-credentials";
    }

    @PostMapping("/login/otp/verify")
    public void verifyOtp(@RequestParam(value = "otpCode", required = false) String otpCode,
                          HttpServletRequest request,
                          HttpServletResponse response,
                          HttpSession session) throws IOException, ServletException {
        OtpChallengeState challenge = readOtpChallenge(session);
        if (challenge == null) {
            response.sendRedirect("/login?error=1&reason=otp-session-missing");
            return;
        }

        String normalizedOtp = otpCode == null ? "" : otpCode.replaceAll("\\D", "");
        if (normalizedOtp.isEmpty()) {
            response.sendRedirect("/login?otp=1&otpError=invalid");
            return;
        }

        AuthService.OtpResult result;
        Long challengeUserId = challenge.userId();
        if (challengeUserId == null || challengeUserId < 1) {
            challengeUserId = parseUserIdFromChallengeToken(challenge.challengeToken());
        }
        try {
            if (challengeUserId != null && challengeUserId > 0) {
                result = authService.verifyOtpForUserId(challengeUserId, normalizedOtp, request);
            } else {
                result = authService.verifyOtpForIdentifier(challenge.identifier(), normalizedOtp, request);
            }
        } catch (ResponseStatusException ex) {
            log.warn("MVC OTP verify failed with status={} reason={} userId={} identifier={}",
                    ex.getStatusCode().value(),
                    ex.getReason(),
                    challengeUserId,
                    challenge.identifier());
            clearOtpChallenge(session);
            HttpStatus status = HttpStatus.resolve(ex.getStatusCode().value());
            if (status == HttpStatus.SERVICE_UNAVAILABLE) {
                response.sendRedirect("/login?error=1&reason=otp-service-unavailable");
                return;
            }
            response.sendRedirect("/login?error=1&reason=otp-expired");
            return;
        } catch (RuntimeException ex) {
            log.error("MVC OTP verify failed with runtime exception userId={} identifier={}",
                    challengeUserId,
                    challenge.identifier(),
                    ex);
            clearOtpChallenge(session);
            response.sendRedirect("/login?error=1&reason=otp-expired");
            return;
        }

        if (result.status() == AuthService.OtpStatus.LOCKED) {
            clearOtpChallenge(session);
            response.sendRedirect("/login?error=1&reason=locked&lockedMinutes=" + safeLockedMinutes(result.lockedMinutes()));
            return;
        }
        if (result.status() == AuthService.OtpStatus.DISABLED) {
            clearOtpChallenge(session);
            response.sendRedirect("/login?error=1&reason=disabled");
            return;
        }
        if (result.status() == AuthService.OtpStatus.INVALID_OTP) {
            response.sendRedirect("/login?otp=1&otpError=invalid");
            return;
        }

        if (result.status() == AuthService.OtpStatus.SUCCESS) {
            clearOtpChallenge(session);
            try {
                completeSessionAuthentication(challenge.identifier(), request, response);
                return;
            } catch (RuntimeException ex) {
                SecurityContextHolder.clearContext();
                response.sendRedirect("/login?error=1&reason=bad-credentials");
                return;
            }
        }

        clearOtpChallenge(session);
        response.sendRedirect("/login?error=1&reason=bad-credentials");
    }

    @PostMapping("/login/otp/back")
    public String otpBack(HttpSession session) {
        clearOtpChallenge(session);
        return "redirect:/login";
    }

    @GetMapping("/login/forgot-password")
    public String forgotPasswordForm() {
        return "auth/forgot-password";
    }

    @PostMapping("/login/forgot-password")
    public String requestPasswordHelp(@RequestParam(value = "username", required = false) String username) {
        loginAssistanceService.requestPasswordHelp(username);
        return "redirect:/login?resetRequested=1";
    }

    @GetMapping("/login/sso")
    public String ssoLogin() {
        boolean ready = loginAssistanceService.requestSsoSignIn();
        if (!ready) {
            return "redirect:/login?ssoUnavailable=1";
        }
        return "redirect:" + loginAssistanceService.ssoAuthorizationPath();
    }

    @GetMapping("/support/contact")
    public String contactSupport() {
        return "support/contact";
    }

    @GetMapping("/legal/privacy")
    public String privacyPolicy() {
        return "legal/privacy";
    }

    @GetMapping("/legal/terms")
    public String termsOfService() {
        return "legal/terms";
    }

    private void completeSessionAuthentication(String identifier,
                                               HttpServletRequest request,
                                               HttpServletResponse response) throws IOException, ServletException {
        UserDetails userDetails = appUserDetailsService.loadUserByUsername(identifier);
        UsernamePasswordAuthenticationToken authentication =
                UsernamePasswordAuthenticationToken.authenticated(userDetails, null, userDetails.getAuthorities());
        authentication.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));

        request.changeSessionId();
        SecurityContext context = SecurityContextHolder.createEmptyContext();
        context.setAuthentication(authentication);
        SecurityContextHolder.setContext(context);
        SECURITY_CONTEXT_REPOSITORY.saveContext(context, request, response);

        loginSuccessHandler.onAuthenticationSuccess(request, response, authentication);
    }

    private void storeOtpChallenge(HttpSession session, String identifier, AuthService.LoginResult result) {
        if (session == null || result == null || result.challengeToken() == null || identifier == null) {
            return;
        }
        session.setAttribute(SESSION_OTP_CHALLENGE, new OtpChallengeState(
                identifier,
                result.userId(),
                result.challengeToken(),
                result.firstTimeSetup(),
                result.otpauthUrl(),
                result.qrDataUrl()
        ));
    }

    private OtpChallengeState readOtpChallenge(HttpSession session) {
        if (session == null) {
            return null;
        }
        Object value = session.getAttribute(SESSION_OTP_CHALLENGE);
        if (value instanceof OtpChallengeState otpChallengeState) {
            return otpChallengeState;
        }
        return null;
    }

    private void clearOtpChallenge(HttpSession session) {
        if (session != null) {
            session.removeAttribute(SESSION_OTP_CHALLENGE);
        }
    }

    private long safeLockedMinutes(Long lockedMinutes) {
        if (lockedMinutes == null || lockedMinutes < 1) {
            return 1;
        }
        return lockedMinutes;
    }

    private String normalizeIdentifier(String identifier) {
        if (identifier == null) {
            return null;
        }
        String normalized = identifier.trim();
        return normalized.isEmpty() ? null : normalized;
    }

    private Long parseUserIdFromChallengeToken(String challengeToken) {
        if (challengeToken == null || challengeToken.isBlank()) {
            return null;
        }
        String[] parts = challengeToken.trim().split("\\.");
        if (parts.length < 2) {
            return null;
        }
        try {
            String payloadJson = new String(Base64.getUrlDecoder().decode(parts[1]), StandardCharsets.UTF_8);
            String marker = "\"sub\":\"";
            int markerIndex = payloadJson.indexOf(marker);
            if (markerIndex < 0) {
                return null;
            }
            int valueStart = markerIndex + marker.length();
            int valueEnd = payloadJson.indexOf('"', valueStart);
            if (valueEnd <= valueStart) {
                return null;
            }
            String subject = payloadJson.substring(valueStart, valueEnd);
            long parsed = Long.parseLong(subject);
            return parsed > 0 ? parsed : null;
        } catch (IllegalArgumentException ex) {
            return null;
        }
    }

    private record OtpChallengeState(String identifier,
                                     Long userId,
                                     String challengeToken,
                                     boolean firstTimeSetup,
                                     String otpauthUrl,
                                     String qrDataUrl) {
    }
}
