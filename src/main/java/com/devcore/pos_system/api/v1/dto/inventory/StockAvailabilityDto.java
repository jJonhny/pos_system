package com.devcore.pos_system.api.v1.dto.inventory;

public record StockAvailabilityDto(
        Long productId,
        String productName,
        String productSku,
        Integer stockQty,
        Integer lowStockThreshold,
        boolean lowStock,
        boolean active,
        boolean allowNegativeStock
) {
}
