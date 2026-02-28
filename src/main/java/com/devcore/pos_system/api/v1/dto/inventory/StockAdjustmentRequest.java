package com.devcore.pos_system.api.v1.dto.inventory;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;

import java.math.BigDecimal;

public record StockAdjustmentRequest(
        @NotNull(message = "productId is required")
        Long productId,

        @NotNull(message = "mode is required")
        AdjustmentMode mode,

        @NotNull(message = "quantity is required")
        @PositiveOrZero(message = "quantity must be >= 0")
        Integer quantity,

        BigDecimal unitCost,
        String currency,
        String reason,
        String referenceId
) {
    public enum AdjustmentMode {
        DELTA,
        TARGET
    }
}
