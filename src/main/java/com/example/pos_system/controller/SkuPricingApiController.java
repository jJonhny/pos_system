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

    /**
     * Executes the SkuPricingApiController operation.
     * <p>Return value: A fully initialized SkuPricingApiController instance.</p>
     *
     * @param skuUnitPricingService Parameter of type {@code SkuUnitPricingService} used by this operation.
     * @param pricingService Parameter of type {@code PricingService} used by this operation.
     * @param inventoryService Parameter of type {@code InventoryService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public SkuPricingApiController(SkuUnitPricingService skuUnitPricingService,
                                   PricingService pricingService,
                                   InventoryService inventoryService) {
        this.skuUnitPricingService = skuUnitPricingService;
        this.pricingService = pricingService;
        this.inventoryService = inventoryService;
    }

    /**
     * Executes the createUnit operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.UnitCreateRequest} used by this operation.
     * @return {@code ResponseEntity<UnitOfMeasure>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createUnit operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.UnitCreateRequest} used by this operation.
     * @return {@code ResponseEntity<UnitOfMeasure>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createUnit operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.UnitCreateRequest} used by this operation.
     * @return {@code ResponseEntity<UnitOfMeasure>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/units")
    public ResponseEntity<UnitOfMeasure> createUnit(@RequestBody VariantApiDtos.UnitCreateRequest request) {
        return ResponseEntity.ok(skuUnitPricingService.createUnit(request));
    }

    /**
     * Executes the createCustomerGroup operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.CustomerGroupCreateRequest} used by this operation.
     * @return {@code ResponseEntity<CustomerGroup>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createCustomerGroup operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.CustomerGroupCreateRequest} used by this operation.
     * @return {@code ResponseEntity<CustomerGroup>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createCustomerGroup operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.CustomerGroupCreateRequest} used by this operation.
     * @return {@code ResponseEntity<CustomerGroup>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/customer-groups")
    public ResponseEntity<CustomerGroup> createCustomerGroup(@RequestBody VariantApiDtos.CustomerGroupCreateRequest request) {
        return ResponseEntity.ok(skuUnitPricingService.createCustomerGroup(request));
    }

    /**
     * Executes the upsertSellUnit operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.SellUnitUpsertRequest} used by this operation.
     * @return {@code ResponseEntity<SkuSellUnit>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the upsertSellUnit operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.SellUnitUpsertRequest} used by this operation.
     * @return {@code ResponseEntity<SkuSellUnit>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the upsertSellUnit operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.SellUnitUpsertRequest} used by this operation.
     * @return {@code ResponseEntity<SkuSellUnit>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/variants/{variantId}/sell-units")
    public ResponseEntity<SkuSellUnit> upsertSellUnit(@PathVariable Long variantId,
                                                      @RequestBody VariantApiDtos.SellUnitUpsertRequest request) {
        return ResponseEntity.ok(skuUnitPricingService.upsertSellUnit(variantId, request));
    }

    /**
     * Executes the updateSellUnit operation.
     *
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.SellUnitUpsertRequest} used by this operation.
     * @return {@code ResponseEntity<SkuSellUnit>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateSellUnit operation.
     *
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.SellUnitUpsertRequest} used by this operation.
     * @return {@code ResponseEntity<SkuSellUnit>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updateSellUnit operation.
     *
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.SellUnitUpsertRequest} used by this operation.
     * @return {@code ResponseEntity<SkuSellUnit>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PutMapping("/sell-units/{sellUnitId}")
    public ResponseEntity<SkuSellUnit> updateSellUnit(@PathVariable Long sellUnitId,
                                                      @RequestBody VariantApiDtos.SellUnitUpsertRequest request) {
        return ResponseEntity.ok(skuUnitPricingService.upsertSellUnitById(sellUnitId, request));
    }

    /**
     * Executes the addBarcode operation.
     *
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.BarcodeCreateRequest} used by this operation.
     * @return {@code ResponseEntity<SkuUnitBarcode>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the addBarcode operation.
     *
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.BarcodeCreateRequest} used by this operation.
     * @return {@code ResponseEntity<SkuUnitBarcode>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the addBarcode operation.
     *
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.BarcodeCreateRequest} used by this operation.
     * @return {@code ResponseEntity<SkuUnitBarcode>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/sell-units/{sellUnitId}/barcodes")
    public ResponseEntity<SkuUnitBarcode> addBarcode(@PathVariable Long sellUnitId,
                                                     @RequestBody VariantApiDtos.BarcodeCreateRequest request) {
        return ResponseEntity.ok(skuUnitPricingService.addBarcode(sellUnitId, request));
    }

    /**
     * Executes the replaceTierPrices operation.
     *
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.TierPriceReplaceRequest} used by this operation.
     * @return {@code ResponseEntity<List<SkuUnitTierPrice>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the replaceTierPrices operation.
     *
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.TierPriceReplaceRequest} used by this operation.
     * @return {@code ResponseEntity<List<SkuUnitTierPrice>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the replaceTierPrices operation.
     *
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.TierPriceReplaceRequest} used by this operation.
     * @return {@code ResponseEntity<List<SkuUnitTierPrice>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PutMapping("/sell-units/{sellUnitId}/tier-prices")
    public ResponseEntity<List<SkuUnitTierPrice>> replaceTierPrices(@PathVariable Long sellUnitId,
                                                                    @RequestBody VariantApiDtos.TierPriceReplaceRequest request) {
        return ResponseEntity.ok(skuUnitPricingService.replaceTierPrices(sellUnitId, request));
    }

    /**
     * Executes the quote operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.PricingQuoteRequest} used by this operation.
     * @return {@code ResponseEntity<VariantApiDtos.PricingQuoteResponse>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the quote operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.PricingQuoteRequest} used by this operation.
     * @return {@code ResponseEntity<VariantApiDtos.PricingQuoteResponse>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the quote operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.PricingQuoteRequest} used by this operation.
     * @return {@code ResponseEntity<VariantApiDtos.PricingQuoteResponse>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/pos/pricing/quote")
    public ResponseEntity<VariantApiDtos.PricingQuoteResponse> quote(@RequestBody VariantApiDtos.PricingQuoteRequest request) {
        return ResponseEntity.ok(pricingService.quote(request));
    }

    /**
     * Executes the deductInventory operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.InventoryDeductRequest} used by this operation.
     * @return {@code ResponseEntity<VariantApiDtos.InventoryDeductResponse>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the deductInventory operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.InventoryDeductRequest} used by this operation.
     * @return {@code ResponseEntity<VariantApiDtos.InventoryDeductResponse>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the deductInventory operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.InventoryDeductRequest} used by this operation.
     * @return {@code ResponseEntity<VariantApiDtos.InventoryDeductResponse>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
