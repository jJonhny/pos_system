package com.example.pos_system.api.v1.dto.product;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public record ApiProductDto(
        Long id,
        String sku,
        String barcode,
        String name,
        BigDecimal price,
        BigDecimal costPrice,
        Integer stockQty,
        Integer lowStockThreshold,
        boolean lowStock,
        boolean active,
        boolean allowNegativeStock,
        Long categoryId,
        String categoryName,
        String imageUrl,
        LocalDateTime updatedAt
) {
}
