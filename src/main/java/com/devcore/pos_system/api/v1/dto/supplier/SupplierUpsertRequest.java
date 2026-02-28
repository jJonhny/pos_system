package com.devcore.pos_system.api.v1.dto.supplier;

import com.devcore.pos_system.entity.SupplierStatus;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record SupplierUpsertRequest(
        @NotBlank(message = "name is required")
        @Size(max = 180, message = "name length must be <= 180")
        String name,

        @Size(max = 60, message = "phone length must be <= 60")
        String phone,

        @Email(message = "email format is invalid")
        @Size(max = 180, message = "email length must be <= 180")
        String email,

        @Size(max = 500, message = "address length must be <= 500")
        String address,

        SupplierStatus status
) {
}
