package com.devcore.pos_system.api.v1.dto.user;

import jakarta.validation.constraints.NotNull;

public record UserStatusUpdateRequest(
        @NotNull(message = "active is required")
        Boolean active
) {
}
