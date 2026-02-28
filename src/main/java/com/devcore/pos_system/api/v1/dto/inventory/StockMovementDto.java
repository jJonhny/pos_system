package com.devcore.pos_system.api.v1.dto.inventory;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public record StockMovementDto(
        Long id,
        Long productId,
        String productName,
        String productSku,
        Integer qtyDelta,
        BigDecimal unitCost,
        String currency,
        String type,
        String refType,
        String refId,
        LocalDateTime createdAt,
        Long actorUserId,
        String terminalId,
        String notes
) {
}
