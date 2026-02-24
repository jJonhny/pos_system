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

    public PricingService(SkuUnitPricingService skuUnitPricingService) {
        this.skuUnitPricingService = skuUnitPricingService;
    }

    public VariantApiDtos.PricingQuoteResponse quote(VariantApiDtos.PricingQuoteRequest request) {
        return skuUnitPricingService.quote(request);
    }

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
