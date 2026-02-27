package com.example.pos_system.api.v1.dto.inventory;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;

import java.math.BigDecimal;

public record StockReceiveRequest(
        @NotNull(message = "productId is required")
        Long productId,

        @NotNull(message = "quantity is required")
        @Positive(message = "quantity must be > 0")
        Integer quantity,

        BigDecimal unitCost,
        String currency,
        String referenceId,
        String notes
) {
}
