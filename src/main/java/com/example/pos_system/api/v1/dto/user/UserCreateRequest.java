package com.example.pos_system.api.v1.dto.user;

import com.example.pos_system.entity.Permission;
import com.example.pos_system.entity.UserRole;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.util.Set;

public record UserCreateRequest(
        @NotBlank(message = "username is required")
        @Size(max = 120, message = "username length must be <= 120")
        String username,

        @Email(message = "email format is invalid")
        @Size(max = 160, message = "email length must be <= 160")
        String email,

        @NotBlank(message = "password is required")
        @Size(min = 8, max = 128, message = "password length must be between 8 and 128")
        String password,

        @NotNull(message = "role is required")
        UserRole role,

        Boolean active,
        Boolean mustResetPassword,
        Boolean mfaRequired,
        Set<Permission> permissions
) {
}
