package com.example.pos_system.service;

import com.example.pos_system.dto.VariantApiDtos;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class VariantGenerationService {
    private final ProductVariantService productVariantService;

    /**
     * Executes the VariantGenerationService operation.
     * <p>Return value: A fully initialized VariantGenerationService instance.</p>
     *
     * @param productVariantService Parameter of type {@code ProductVariantService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public VariantGenerationService(ProductVariantService productVariantService) {
        this.productVariantService = productVariantService;
    }

    /**
     * Executes the generateAndMerge operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantGenerateRequest} used by this operation.
     * @return {@code VariantApiDtos.VariantGenerationResult} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public VariantApiDtos.VariantGenerationResult generateAndMerge(Long productId,
                                                                   VariantApiDtos.VariantGenerateRequest request) {
        return productVariantService.generateVariants(productId, request);
    }
}
