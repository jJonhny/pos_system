package com.example.pos_system.api.v1.dto.user;

import java.time.LocalDateTime;
import java.util.List;

public record ApiUserDto(
        Long id,
        String username,
        String email,
        String role,
        boolean active,
        boolean mustResetPassword,
        boolean mfaRequired,
        List<String> permissions,
        LocalDateTime lastLoginAt
) {
}
