package com.devcore.pos_system.api.v1.dto.user;

import com.devcore.pos_system.entity.UserRole;
import jakarta.validation.constraints.NotNull;

public record UserRoleUpdateRequest(
        @NotNull(message = "role is required")
        UserRole role
) {
}
