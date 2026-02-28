package com.devcore.pos_system.controller;

import com.devcore.pos_system.dto.VariantApiDtos;
import com.devcore.pos_system.entity.AttributeGroup;
import com.devcore.pos_system.entity.AttributeValue;
import com.devcore.pos_system.entity.ProductVariant;
import com.devcore.pos_system.entity.ProductVariantExclusion;
import com.devcore.pos_system.service.ProductVariantService;
import com.devcore.pos_system.service.VariantGenerationService;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1")
public class VariantAdminApiController {
    private final ProductVariantService productVariantService;
    private final VariantGenerationService variantGenerationService;

    /**
     * Executes the VariantAdminApiController operation.
     * <p>Return value: A fully initialized VariantAdminApiController instance.</p>
     *
     * @param productVariantService Parameter of type {@code ProductVariantService} used by this operation.
     * @param variantGenerationService Parameter of type {@code VariantGenerationService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public VariantAdminApiController(ProductVariantService productVariantService,
                                     VariantGenerationService variantGenerationService) {
        this.productVariantService = productVariantService;
        this.variantGenerationService = variantGenerationService;
    }

    /**
     * Executes the createAttributeGroup operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.AttributeGroupCreateRequest} used by this operation.
     * @return {@code ResponseEntity<AttributeGroup>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createAttributeGroup operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.AttributeGroupCreateRequest} used by this operation.
     * @return {@code ResponseEntity<AttributeGroup>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createAttributeGroup operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.AttributeGroupCreateRequest} used by this operation.
     * @return {@code ResponseEntity<AttributeGroup>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/attributes/groups")
    public ResponseEntity<AttributeGroup> createAttributeGroup(@RequestBody VariantApiDtos.AttributeGroupCreateRequest request) {
        return ResponseEntity.ok(productVariantService.createAttributeGroup(request));
    }

    /**
     * Executes the deleteAttributeGroup operation.
     *
     * @param groupId Parameter of type {@code Long} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the deleteAttributeGroup operation.
     *
     * @param groupId Parameter of type {@code Long} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the deleteAttributeGroup operation.
     *
     * @param groupId Parameter of type {@code Long} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @DeleteMapping("/attributes/groups/{groupId}")
    public ResponseEntity<Void> deleteAttributeGroup(@PathVariable Long groupId) {
        productVariantService.deleteAttributeGroup(groupId);
        return ResponseEntity.noContent().build();
    }

    /**
     * Executes the createAttributeValue operation.
     *
     * @param groupId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.AttributeValueCreateRequest} used by this operation.
     * @return {@code ResponseEntity<AttributeValue>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createAttributeValue operation.
     *
     * @param groupId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.AttributeValueCreateRequest} used by this operation.
     * @return {@code ResponseEntity<AttributeValue>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createAttributeValue operation.
     *
     * @param groupId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.AttributeValueCreateRequest} used by this operation.
     * @return {@code ResponseEntity<AttributeValue>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/attributes/groups/{groupId}/values")
    public ResponseEntity<AttributeValue> createAttributeValue(@PathVariable Long groupId,
                                                               @RequestBody VariantApiDtos.AttributeValueCreateRequest request) {
        return ResponseEntity.ok(productVariantService.createAttributeValue(groupId, request));
    }

    /**
     * Executes the deleteAttributeValue operation.
     *
     * @param valueId Parameter of type {@code Long} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the deleteAttributeValue operation.
     *
     * @param valueId Parameter of type {@code Long} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the deleteAttributeValue operation.
     *
     * @param valueId Parameter of type {@code Long} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @DeleteMapping("/attributes/values/{valueId}")
    public ResponseEntity<Void> deleteAttributeValue(@PathVariable Long valueId) {
        productVariantService.deleteAttributeValue(valueId);
        return ResponseEntity.noContent().build();
    }

    /**
     * Executes the configureProductAttributes operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.ProductAttributeConfigRequest} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the configureProductAttributes operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.ProductAttributeConfigRequest} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the configureProductAttributes operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.ProductAttributeConfigRequest} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PutMapping("/products/{productId}/attribute-config")
    public ResponseEntity<Void> configureProductAttributes(@PathVariable Long productId,
                                                           @RequestBody VariantApiDtos.ProductAttributeConfigRequest request) {
        productVariantService.configureProductAttributes(productId, request);
        return ResponseEntity.noContent().build();
    }

    /**
     * Executes the generateVariants operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantGenerateRequest} used by this operation.
     * @return {@code ResponseEntity<VariantApiDtos.VariantGenerationResult>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the generateVariants operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantGenerateRequest} used by this operation.
     * @return {@code ResponseEntity<VariantApiDtos.VariantGenerationResult>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the generateVariants operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantGenerateRequest} used by this operation.
     * @return {@code ResponseEntity<VariantApiDtos.VariantGenerationResult>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/products/{productId}/variants/generate")
    public ResponseEntity<VariantApiDtos.VariantGenerationResult> generateVariants(@PathVariable Long productId,
                                                                                    @RequestBody(required = false) VariantApiDtos.VariantGenerateRequest request) {
        return ResponseEntity.ok(variantGenerationService.generateAndMerge(productId, request));
    }

    /**
     * Executes the addExclusion operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantExclusionRequest} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @return {@code ResponseEntity<ProductVariantExclusion>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the addExclusion operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantExclusionRequest} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @return {@code ResponseEntity<ProductVariantExclusion>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the addExclusion operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantExclusionRequest} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @return {@code ResponseEntity<ProductVariantExclusion>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/products/{productId}/variant-exclusions")
    public ResponseEntity<ProductVariantExclusion> addExclusion(@PathVariable Long productId,
                                                                @RequestBody VariantApiDtos.VariantExclusionRequest request,
                                                                Authentication authentication) {
        return ResponseEntity.ok(productVariantService.addExclusion(productId, request, authentication));
    }

    /**
     * Executes the removeExclusion operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param exclusionId Parameter of type {@code Long} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the removeExclusion operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param exclusionId Parameter of type {@code Long} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the removeExclusion operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param exclusionId Parameter of type {@code Long} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @DeleteMapping("/products/{productId}/variant-exclusions/{exclusionId}")
    public ResponseEntity<Void> removeExclusion(@PathVariable Long productId, @PathVariable Long exclusionId) {
        productVariantService.removeExclusion(productId, exclusionId);
        return ResponseEntity.noContent().build();
    }

    /**
     * Executes the updateVariantState operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantStateUpdateRequest} used by this operation.
     * @return {@code ResponseEntity<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateVariantState operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantStateUpdateRequest} used by this operation.
     * @return {@code ResponseEntity<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateVariantState operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantStateUpdateRequest} used by this operation.
     * @return {@code ResponseEntity<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PatchMapping("/variants/{variantId}/state")
    public ResponseEntity<ProductVariant> updateVariantState(@PathVariable Long variantId,
                                                             @RequestBody VariantApiDtos.VariantStateUpdateRequest request) {
        return ResponseEntity.ok(productVariantService.updateVariantState(variantId, request));
    }
}
