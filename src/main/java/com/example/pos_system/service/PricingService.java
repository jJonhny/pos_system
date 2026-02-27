package com.example.pos_system.service;

import com.example.pos_system.dto.VariantApiDtos;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;

@Service
@Transactional(readOnly = true)
public class PricingService {
    private final SkuUnitPricingService skuUnitPricingService;

    /**
     * Executes the PricingService operation.
     * <p>Return value: A fully initialized PricingService instance.</p>
     *
     * @param skuUnitPricingService Parameter of type {@code SkuUnitPricingService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public PricingService(SkuUnitPricingService skuUnitPricingService) {
        this.skuUnitPricingService = skuUnitPricingService;
    }

    /**
     * Executes the quote operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.PricingQuoteRequest} used by this operation.
     * @return {@code VariantApiDtos.PricingQuoteResponse} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public VariantApiDtos.PricingQuoteResponse quote(VariantApiDtos.PricingQuoteRequest request) {
        return skuUnitPricingService.quote(request);
    }

    /**
     * Executes the quoteLine operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param qty Parameter of type {@code BigDecimal} used by this operation.
     * @param customerGroupCode Parameter of type {@code String} used by this operation.
     * @param currencyCode Parameter of type {@code String} used by this operation.
     * @return {@code VariantApiDtos.PricingQuoteLineResponse} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public VariantApiDtos.PricingQuoteLineResponse quoteLine(Long variantId,
                                                             Long sellUnitId,
                                                             BigDecimal qty,
                                                             String customerGroupCode,
                                                             String currencyCode) {
        VariantApiDtos.PricingQuoteRequest request = new VariantApiDtos.PricingQuoteRequest(
                customerGroupCode,
                currencyCode,
                List.of(new VariantApiDtos.PricingQuoteLineRequest(variantId, sellUnitId, qty))
        );
        VariantApiDtos.PricingQuoteResponse response = skuUnitPricingService.quote(request);
        if (response.lines() == null || response.lines().isEmpty()) {
            throw new IllegalStateException("Pricing quote returned no lines.");
        }
        return response.lines().getFirst();
    }
}
