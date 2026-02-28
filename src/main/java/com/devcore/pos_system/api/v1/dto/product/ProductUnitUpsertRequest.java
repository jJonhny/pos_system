package com.devcore.pos_system.api.v1.dto.product;

import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import java.math.BigDecimal;

public record ProductUnitUpsertRequest(
        @NotBlank(message = "name is required")
        @Size(max = 64, message = "name length must be <= 64")
        String name,

        @Size(max = 32, message = "abbreviation length must be <= 32")
        String abbreviation,

        @DecimalMin(value = "0.000001", message = "conversionToBase must be > 0")
        BigDecimal conversionToBase,

        Boolean allowForSale,
        Boolean allowForPurchase,
        Boolean defaultSaleUnit,
        Boolean defaultPurchaseUnit,

        @Size(max = 128, message = "barcode length must be <= 128")
        String barcode
) {
}
