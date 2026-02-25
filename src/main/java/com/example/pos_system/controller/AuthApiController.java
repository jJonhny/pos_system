package com.example.pos_system.controller;

import com.example.pos_system.entity.AppUser;
import com.example.pos_system.repository.AppUserRepo;
import com.example.pos_system.service.AuthService;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;

import java.util.List;

@RestController
@RequestMapping("/api/v1/auth")
public class AuthApiController {
    private final AuthService authService;
    private final AppUserRepo appUserRepo;

    public AuthApiController(AuthService authService, AppUserRepo appUserRepo) {
        this.authService = authService;
        this.appUserRepo = appUserRepo;
    }

    @PostMapping("/register")
    public ResponseEntity<RegisterResponse> register(@RequestBody RegisterRequest request) {
        AuthService.RegisterResult result =
                authService.register(request.email(), request.password(), request.role());
        return ResponseEntity.status(HttpStatus.CREATED).body(new RegisterResponse(
                result.userId(),
                result.username(),
                result.email(),
                result.role().name(),
                result.permissions()
        ));
    }

    @PostMapping("/login")
    public ResponseEntity<LoginResponse> login(@RequestBody LoginRequest request, HttpServletRequest servletRequest) {
        AuthService.LoginResult result = authService.loginWithPassword(request.email(), request.password(), servletRequest);

        if (result.status() == AuthService.LoginStatus.LOCKED) {
            return ResponseEntity.status(HttpStatus.LOCKED).body(new LoginResponse(
                    result.status().name(),
                    "Account locked due to repeated failures.",
                    null,
                    false,
                    null,
                    null,
                    result.lockedMinutes()
            ));
        }
        if (result.status() == AuthService.LoginStatus.DISABLED) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(new LoginResponse(
                    result.status().name(),
                    "Account is disabled.",
                    null,
                    false,
                    null,
                    null,
                    null
            ));
        }
        if (result.status() == AuthService.LoginStatus.INVALID_CREDENTIALS) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(new LoginResponse(
                    result.status().name(),
                    "Invalid email or password.",
                    null,
                    false,
                    null,
                    null,
                    null
            ));
        }

        return ResponseEntity.ok(new LoginResponse(
                result.status().name(),
                "Password verified. Complete OTP verification.",
                result.challengeToken(),
                result.firstTimeSetup(),
                result.otpauthUrl(),
                result.qrDataUrl(),
                null
        ));
    }

    @PostMapping("/verify-otp")
    public ResponseEntity<OtpVerifyResponse> verifyOtp(@RequestBody OtpVerifyRequest request,
                                                       HttpServletRequest servletRequest) {
        AuthService.OtpResult result = authService.verifyOtp(request.challengeToken(), request.otpCode(), servletRequest);

        if (result.status() == AuthService.OtpStatus.LOCKED) {
            return ResponseEntity.status(HttpStatus.LOCKED).body(new OtpVerifyResponse(
                    result.status().name(),
                    "Account locked due to repeated failures.",
                    null,
                    null,
                    0,
                    List.of(),
                    result.lockedMinutes()
            ));
        }
        if (result.status() == AuthService.OtpStatus.DISABLED) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(new OtpVerifyResponse(
                    result.status().name(),
                    "Account is disabled.",
                    null,
                    null,
                    0,
                    List.of(),
                    null
            ));
        }
        if (result.status() == AuthService.OtpStatus.INVALID_OTP) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(new OtpVerifyResponse(
                    result.status().name(),
                    "Invalid OTP code.",
                    null,
                    null,
                    0,
                    List.of(),
                    null
            ));
        }

        return ResponseEntity.ok(new OtpVerifyResponse(
                result.status().name(),
                "OTP verified. Login successful.",
                result.accessToken(),
                result.role() == null ? null : result.role().name(),
                result.expiresInSeconds(),
                result.permissions(),
                null
        ));
    }

    @GetMapping("/me")
    public AuthProfileResponse me(Authentication authentication) {
        if (authentication == null || authentication.getName() == null || authentication.getName().isBlank()) {
            throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Authentication required.");
        }
        AppUser user = appUserRepo.findByUsernameIgnoreCaseOrEmailIgnoreCase(authentication.getName(), authentication.getName())
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.UNAUTHORIZED, "User not found."));
        List<String> permissions = user.getPermissions() == null
                ? List.of()
                : user.getPermissions().stream().map(Enum::name).sorted().toList();
        return new AuthProfileResponse(
                user.getId(),
                user.getUsername(),
                user.getEmail(),
                user.getRole() == null ? null : user.getRole().name(),
                permissions
        );
    }

    public record RegisterRequest(String email, String password, String role) {
    }

    public record LoginRequest(String email, String password) {
    }

    public record OtpVerifyRequest(String challengeToken, String otpCode) {
    }

    public record RegisterResponse(Long userId,
                                   String username,
                                   String email,
                                   String role,
                                   List<String> permissions) {
    }

    public record LoginResponse(String status,
                                String message,
                                String challengeToken,
                                boolean firstTimeSetup,
                                String otpauthUrl,
                                String qrDataUrl,
                                Long lockedMinutes) {
    }

    public record OtpVerifyResponse(String status,
                                    String message,
                                    String accessToken,
                                    String role,
                                    long expiresInSeconds,
                                    List<String> permissions,
                                    Long lockedMinutes) {
    }

    public record AuthProfileResponse(Long userId,
                                      String username,
                                      String email,
                                      String role,
                                      List<String> permissions) {
    }
}
