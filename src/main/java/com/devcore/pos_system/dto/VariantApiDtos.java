package com.devcore.pos_system.dto;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

public final class VariantApiDtos {
    /**
     * Executes the VariantApiDtos operation.
     * <p>Return value: A fully initialized VariantApiDtos instance.</p>
     *
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private VariantApiDtos() {
    }

    public record AttributeGroupCreateRequest(String code, String name, Integer sortOrder) {}

    public record AttributeValueCreateRequest(String code, String displayName, Integer sortOrder) {}

    public record ProductAttributeConfigRequest(List<ProductAttributeGroupSelection> groups,
                                                List<ProductAttributeAllowedValues> allowedValues) {}

    public record ProductAttributeGroupSelection(Long groupId, Integer sortOrder, Boolean required) {}

    public record ProductAttributeAllowedValues(Long groupId, List<Long> valueIds) {}

    public record VariantGenerateRequest(String mode,
                                         List<String> preserveFields,
                                         DefaultVariantValues defaultValues,
                                         Integer maxVariants) {}

    public record DefaultVariantValues(Boolean enabled, BigDecimal price, BigDecimal cost) {}

    public record VariantExclusionRequest(String combinationKey, String reason) {}

    public record VariantStateUpdateRequest(Boolean enabled, Boolean impossible) {}

    public record VariantGenerationResult(Long productId,
                                          int expectedVariants,
                                          int created,
                                          int updated,
                                          int archived,
                                          int skippedExcluded) {}

    public record SellUnitUpsertRequest(String unitCode,
                                        BigDecimal conversionToBase,
                                        Boolean isBase,
                                        BigDecimal basePrice,
                                        Boolean enabled) {}

    public record UnitCreateRequest(String code, String name, Integer precisionScale) {}

    public record CustomerGroupCreateRequest(String code, String name, Integer priority) {}

    public record BarcodeCreateRequest(String barcode, Boolean isPrimary) {}

    public record TierPriceItem(String customerGroupCode,
                                BigDecimal minQty,
                                BigDecimal unitPrice,
                                String currencyCode,
                                LocalDateTime effectiveFrom,
                                LocalDateTime effectiveTo,
                                Boolean active) {}

    public record TierPriceReplaceRequest(String currencyCode, List<TierPriceItem> tiers) {}

    public record PricingQuoteRequest(String customerGroupCode, String currencyCode, List<PricingQuoteLineRequest> lines) {}

    public record PricingQuoteLineRequest(Long variantId, Long sellUnitId, BigDecimal qty) {}

    public record PricingQuoteLineResponse(Long variantId,
                                           Long sellUnitId,
                                           BigDecimal qty,
                                           String priceSource,
                                           AppliedTier appliedTier,
                                           BigDecimal unitPrice,
                                           BigDecimal lineSubtotal,
                                           BigDecimal inventoryBaseQty) {}

    public record AppliedTier(String customerGroupCode, BigDecimal minQty) {}

    public record PricingQuoteResponse(List<PricingQuoteLineResponse> lines,
                                       BigDecimal subtotal,
                                       String currencyCode) {}

    public record InventoryDeductRequest(Long sellUnitId, BigDecimal qty) {}

    public record InventoryDeductResponse(Long variantId,
                                          Long sellUnitId,
                                          BigDecimal soldQty,
                                          BigDecimal deductedBaseQty,
                                          BigDecimal remainingBaseQty) {}

    public record IdResponse(Long id) {}
}
