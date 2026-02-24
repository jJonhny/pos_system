package com.example.pos_system.service;

import com.example.pos_system.dto.VariantApiDtos;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class VariantGenerationService {
    private final ProductVariantService productVariantService;

    public VariantGenerationService(ProductVariantService productVariantService) {
        this.productVariantService = productVariantService;
    }

    public VariantApiDtos.VariantGenerationResult generateAndMerge(Long productId,
                                                                   VariantApiDtos.VariantGenerateRequest request) {
        return productVariantService.generateVariants(productId, request);
    }
}
