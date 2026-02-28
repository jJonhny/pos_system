package com.devcore.pos_system.api.v1.dto.product;

import java.math.BigDecimal;

public record ApiProductUnitDto(
        Long id,
        String name,
        String abbreviation,
        BigDecimal conversionToBase,
        boolean allowForSale,
        boolean allowForPurchase,
        boolean defaultSaleUnit,
        boolean defaultPurchaseUnit,
        String barcode
) {
}
