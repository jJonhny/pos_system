package com.example.pos_system.controller;

import com.example.pos_system.dto.VariantApiDtos;
import com.example.pos_system.entity.CustomerGroup;
import com.example.pos_system.entity.SkuSellUnit;
import com.example.pos_system.entity.SkuUnitBarcode;
import com.example.pos_system.entity.SkuUnitTierPrice;
import com.example.pos_system.entity.UnitOfMeasure;
import com.example.pos_system.service.InventoryService;
import com.example.pos_system.service.PricingService;
import com.example.pos_system.service.SkuUnitPricingService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/api/v1")
public class SkuPricingApiController {
    private final SkuUnitPricingService skuUnitPricingService;
    private final PricingService pricingService;
    private final InventoryService inventoryService;

    public SkuPricingApiController(SkuUnitPricingService skuUnitPricingService,
                                   PricingService pricingService,
                                   InventoryService inventoryService) {
        this.skuUnitPricingService = skuUnitPricingService;
        this.pricingService = pricingService;
        this.inventoryService = inventoryService;
    }

    @PostMapping("/units")
    public ResponseEntity<UnitOfMeasure> createUnit(@RequestBody VariantApiDtos.UnitCreateRequest request) {
        return ResponseEntity.ok(skuUnitPricingService.createUnit(request));
    }

    @PostMapping("/customer-groups")
    public ResponseEntity<CustomerGroup> createCustomerGroup(@RequestBody VariantApiDtos.CustomerGroupCreateRequest request) {
        return ResponseEntity.ok(skuUnitPricingService.createCustomerGroup(request));
    }

    @PostMapping("/variants/{variantId}/sell-units")
    public ResponseEntity<SkuSellUnit> upsertSellUnit(@PathVariable Long variantId,
                                                      @RequestBody VariantApiDtos.SellUnitUpsertRequest request) {
        return ResponseEntity.ok(skuUnitPricingService.upsertSellUnit(variantId, request));
    }

    @PutMapping("/sell-units/{sellUnitId}")
    public ResponseEntity<SkuSellUnit> updateSellUnit(@PathVariable Long sellUnitId,
                                                      @RequestBody VariantApiDtos.SellUnitUpsertRequest request) {
        return ResponseEntity.ok(skuUnitPricingService.upsertSellUnitById(sellUnitId, request));
    }

    @PostMapping("/sell-units/{sellUnitId}/barcodes")
    public ResponseEntity<SkuUnitBarcode> addBarcode(@PathVariable Long sellUnitId,
                                                     @RequestBody VariantApiDtos.BarcodeCreateRequest request) {
        return ResponseEntity.ok(skuUnitPricingService.addBarcode(sellUnitId, request));
    }

    @PutMapping("/sell-units/{sellUnitId}/tier-prices")
    public ResponseEntity<List<SkuUnitTierPrice>> replaceTierPrices(@PathVariable Long sellUnitId,
                                                                    @RequestBody VariantApiDtos.TierPriceReplaceRequest request) {
        return ResponseEntity.ok(skuUnitPricingService.replaceTierPrices(sellUnitId, request));
    }

    @PostMapping("/pos/pricing/quote")
    public ResponseEntity<VariantApiDtos.PricingQuoteResponse> quote(@RequestBody VariantApiDtos.PricingQuoteRequest request) {
        return ResponseEntity.ok(pricingService.quote(request));
    }

    @PostMapping("/inventory/deduct")
    public ResponseEntity<VariantApiDtos.InventoryDeductResponse> deductInventory(
            @RequestBody VariantApiDtos.InventoryDeductRequest request) {
        InventoryService.VariantUnitDeductionResult result =
                inventoryService.deductVariantUnitStock(
                        request == null ? null : request.sellUnitId(),
                        request == null ? null : request.qty()
                );
        return ResponseEntity.ok(new VariantApiDtos.InventoryDeductResponse(
                result.variantId(),
                result.sellUnitId(),
                result.soldQty(),
                result.deductedBaseQty(),
                result.remainingBaseQty()
        ));
    }
}
