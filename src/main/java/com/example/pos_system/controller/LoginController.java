package com.example.pos_system.controller;

import com.example.pos_system.service.LoginAssistanceService;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class LoginController {
    private final LoginAssistanceService loginAssistanceService;

    public LoginController(LoginAssistanceService loginAssistanceService) {
        this.loginAssistanceService = loginAssistanceService;
    }

    @GetMapping("/login")
    public String login() {
        return "login";
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
        loginAssistanceService.requestSsoSignIn();
        return "redirect:/login?ssoUnavailable=1";
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
}
