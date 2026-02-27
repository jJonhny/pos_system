package com.example.pos_system.api.v1.dto.product;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.PositiveOrZero;
import jakarta.validation.constraints.Size;

import java.math.BigDecimal;

public record ProductCreateRequest(
        @Size(max = 120, message = "sku length must be <= 120")
        String sku,

        @Size(max = 120, message = "barcode length must be <= 120")
        String barcode,

        @NotBlank(message = "name is required")
        @Size(max = 255, message = "name length must be <= 255")
        String name,

        @PositiveOrZero(message = "price must be >= 0")
        BigDecimal price,

        @PositiveOrZero(message = "costPrice must be >= 0")
        BigDecimal costPrice,

        @PositiveOrZero(message = "lowStockThreshold must be >= 0")
        Integer lowStockThreshold,

        Long categoryId,
        Boolean active,
        Boolean allowNegativeStock,

        @Size(max = 2048, message = "imageUrl length must be <= 2048")
        String imageUrl
) {
}
