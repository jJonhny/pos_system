package com.example.pos_system.controller;

import com.example.pos_system.dto.VariantApiDtos;
import com.example.pos_system.entity.AttributeGroup;
import com.example.pos_system.entity.AttributeValue;
import com.example.pos_system.entity.ProductVariant;
import com.example.pos_system.entity.ProductVariantExclusion;
import com.example.pos_system.service.ProductVariantService;
import com.example.pos_system.service.VariantGenerationService;
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

    public VariantAdminApiController(ProductVariantService productVariantService,
                                     VariantGenerationService variantGenerationService) {
        this.productVariantService = productVariantService;
        this.variantGenerationService = variantGenerationService;
    }

    @PostMapping("/attributes/groups")
    public ResponseEntity<AttributeGroup> createAttributeGroup(@RequestBody VariantApiDtos.AttributeGroupCreateRequest request) {
        return ResponseEntity.ok(productVariantService.createAttributeGroup(request));
    }

    @DeleteMapping("/attributes/groups/{groupId}")
    public ResponseEntity<Void> deleteAttributeGroup(@PathVariable Long groupId) {
        productVariantService.deleteAttributeGroup(groupId);
        return ResponseEntity.noContent().build();
    }

    @PostMapping("/attributes/groups/{groupId}/values")
    public ResponseEntity<AttributeValue> createAttributeValue(@PathVariable Long groupId,
                                                               @RequestBody VariantApiDtos.AttributeValueCreateRequest request) {
        return ResponseEntity.ok(productVariantService.createAttributeValue(groupId, request));
    }

    @DeleteMapping("/attributes/values/{valueId}")
    public ResponseEntity<Void> deleteAttributeValue(@PathVariable Long valueId) {
        productVariantService.deleteAttributeValue(valueId);
        return ResponseEntity.noContent().build();
    }

    @PutMapping("/products/{productId}/attribute-config")
    public ResponseEntity<Void> configureProductAttributes(@PathVariable Long productId,
                                                           @RequestBody VariantApiDtos.ProductAttributeConfigRequest request) {
        productVariantService.configureProductAttributes(productId, request);
        return ResponseEntity.noContent().build();
    }

    @PostMapping("/products/{productId}/variants/generate")
    public ResponseEntity<VariantApiDtos.VariantGenerationResult> generateVariants(@PathVariable Long productId,
                                                                                    @RequestBody(required = false) VariantApiDtos.VariantGenerateRequest request) {
        return ResponseEntity.ok(variantGenerationService.generateAndMerge(productId, request));
    }

    @PostMapping("/products/{productId}/variant-exclusions")
    public ResponseEntity<ProductVariantExclusion> addExclusion(@PathVariable Long productId,
                                                                @RequestBody VariantApiDtos.VariantExclusionRequest request,
                                                                Authentication authentication) {
        return ResponseEntity.ok(productVariantService.addExclusion(productId, request, authentication));
    }

    @DeleteMapping("/products/{productId}/variant-exclusions/{exclusionId}")
    public ResponseEntity<Void> removeExclusion(@PathVariable Long productId, @PathVariable Long exclusionId) {
        productVariantService.removeExclusion(productId, exclusionId);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/variants/{variantId}/state")
    public ResponseEntity<ProductVariant> updateVariantState(@PathVariable Long variantId,
                                                             @RequestBody VariantApiDtos.VariantStateUpdateRequest request) {
        return ResponseEntity.ok(productVariantService.updateVariantState(variantId, request));
    }
}
