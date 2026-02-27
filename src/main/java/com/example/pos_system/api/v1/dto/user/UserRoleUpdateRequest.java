package com.example.pos_system.api.v1.dto.user;

import com.example.pos_system.entity.UserRole;
import jakarta.validation.constraints.NotNull;

public record UserRoleUpdateRequest(
        @NotNull(message = "role is required")
        UserRole role
) {
}
