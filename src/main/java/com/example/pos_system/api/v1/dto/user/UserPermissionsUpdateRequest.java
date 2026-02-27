package com.example.pos_system.api.v1.dto.user;

import com.example.pos_system.entity.Permission;
import jakarta.validation.constraints.NotNull;

import java.util.Set;

public record UserPermissionsUpdateRequest(
        @NotNull(message = "permissions is required")
        Set<Permission> permissions
) {
}
