package com.devcore.pos_system.api.v1.dto.product;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

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
        String baseUnitName,
        Integer baseUnitPrecision,
        Long retailPriceUnitId,
        Long wholesalePriceUnitId,
        Long wholesaleMinQtyUnitId,
        Long lowStockThresholdUnitId,
        Integer unitsPerBox,
        Integer unitsPerCase,
        Long categoryId,
        String categoryName,
        String imageUrl,
        LocalDateTime updatedAt,
        List<ApiProductUnitDto> units
) {
}
