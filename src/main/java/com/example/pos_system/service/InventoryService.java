package com.example.pos_system.service;

import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.ProductVariant;
import com.example.pos_system.entity.SkuSellUnit;
import com.example.pos_system.entity.StockMovementType;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.SkuSellUnitRepo;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Service
@Transactional
public class InventoryService {
    private final ProductRepo productRepo;
    private final AuditEventService auditEventService;
    private final StockMovementService stockMovementService;
    private final SkuSellUnitRepo skuSellUnitRepo;
    private final VariantInventoryService variantInventoryService;

    /**
     * Executes the InventoryService operation.
     * <p>Return value: A fully initialized InventoryService instance.</p>
     *
     * @param productRepo Parameter of type {@code ProductRepo} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * @param stockMovementService Parameter of type {@code StockMovementService} used by this operation.
     * @param skuSellUnitRepo Parameter of type {@code SkuSellUnitRepo} used by this operation.
     * @param variantInventoryService Parameter of type {@code VariantInventoryService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public InventoryService(ProductRepo productRepo, AuditEventService auditEventService,
                            StockMovementService stockMovementService,
                            SkuSellUnitRepo skuSellUnitRepo,
                            VariantInventoryService variantInventoryService) {
        this.productRepo = productRepo;
        this.auditEventService = auditEventService;
        this.stockMovementService = stockMovementService;
        this.skuSellUnitRepo = skuSellUnitRepo;
        this.variantInventoryService = variantInventoryService;
    }

    /**
     * Executes the quickUpdate operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param price Parameter of type {@code String} used by this operation.
     * @param stockQty Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Product quickUpdate(Long id, String price, String stockQty) {
        Product product = productRepo.findByIdForUpdate(id).orElseThrow();
        Map<String, Object> before = productSnapshot(product);
        boolean changedPrice = false;
        boolean changedStock = false;
        Integer parsedStock = null;

        if (hasText(price)) {
            BigDecimal parsedPrice = parseBigDecimal(price);
            if (parsedPrice == null || parsedPrice.compareTo(BigDecimal.ZERO) < 0) {
                throw new IllegalArgumentException("Invalid price value.");
            }
            if (product.getPrice() == null || product.getPrice().compareTo(parsedPrice) != 0) {
                product.setPrice(parsedPrice);
                changedPrice = true;
            }
        }

        if (hasText(stockQty)) {
            parsedStock = parseInteger(stockQty);
            if (parsedStock == null || parsedStock < 0) {
                throw new IllegalArgumentException("Invalid stock quantity.");
            }
            int current = safeStock(product.getStockQty());
            if (current != parsedStock) {
                changedStock = true;
            }
        }

        if (!changedPrice && !changedStock) {
            throw new IllegalArgumentException("No changes provided for quick update.");
        }

        Product saved = changedPrice ? productRepo.save(product) : product;
        if (changedStock && parsedStock != null) {
            saved = stockMovementService.adjustToTarget(
                    saved.getId(),
                    parsedStock,
                    saved.getCostPrice(),
                    null,
                    StockMovementType.ADJUSTMENT,
                    "ADJ",
                    String.valueOf(saved.getId()),
                    null,
                    "Quick stock update"
            );
        }

        Map<String, Object> after = productSnapshot(saved);
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("mode", "quick-update");
        metadata.put("changedPrice", changedPrice);
        metadata.put("changedStock", changedStock);

        if (changedStock) {
            auditEventService.record("STOCK_ADJUSTMENT", "PRODUCT", saved.getId(), before, after, metadata);
        }
        if (changedPrice) {
            auditEventService.record("PRICE_OVERRIDE", "PRODUCT", saved.getId(), before, after, metadata);
        }
        return saved;
    }

    /**
     * Executes the deductVariantUnitStock operation.
     *
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param qty Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code VariantUnitDeductionResult} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public VariantUnitDeductionResult deductVariantUnitStock(Long sellUnitId, BigDecimal qty) {
        if (sellUnitId == null) {
            throw new IllegalArgumentException("sellUnitId is required.");
        }
        BigDecimal soldQty = normalizeQty(qty);
        if (soldQty.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("qty must be positive.");
        }

        SkuSellUnit sellUnit = skuSellUnitRepo.findById(sellUnitId)
                .orElseThrow(() -> new IllegalArgumentException("Sell unit not found."));
        if (!Boolean.TRUE.equals(sellUnit.getEnabled())) {
            throw new IllegalStateException("Sell unit is disabled.");
        }
        ProductVariant variant = sellUnit.getVariant();
        if (variant == null || variant.getId() == null) {
            throw new IllegalStateException("Variant not found for sell unit.");
        }
        if (Boolean.TRUE.equals(variant.getArchived())
                || Boolean.TRUE.equals(variant.getImpossible())
                || !Boolean.TRUE.equals(variant.getEnabled())) {
            throw new IllegalStateException("Variant is not saleable.");
        }
        if (variant.getProduct() == null || Boolean.FALSE.equals(variant.getProduct().getActive())) {
            throw new IllegalStateException("Product is inactive.");
        }

        BigDecimal conversion = normalizeQty(sellUnit.getConversionToBase());
        if (conversion.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalStateException("Sell unit conversion must be positive.");
        }
        BigDecimal deductedBaseQty = soldQty.multiply(conversion).setScale(6, RoundingMode.HALF_UP);

        BigDecimal beforeStock = normalizeQty(variant.getStockBaseQty());
        ProductVariant updated = variantInventoryService.recordSale(variant.getId(), deductedBaseQty);
        BigDecimal afterStock = normalizeQty(updated.getStockBaseQty());

        Map<String, Object> before = variantSnapshot(variant.getId(), beforeStock);
        Map<String, Object> after = variantSnapshot(updated.getId(), afterStock);
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("sellUnitId", sellUnit.getId());
        metadata.put("unitCode", sellUnit.getUnit() == null ? null : sellUnit.getUnit().getCode());
        metadata.put("soldQty", soldQty);
        metadata.put("conversionToBase", conversion);
        metadata.put("deductedBaseQty", deductedBaseQty);
        auditEventService.record("SKU_STOCK_DEDUCT", "PRODUCT_VARIANT", updated.getId(), before, after, metadata);

        return new VariantUnitDeductionResult(
                updated.getId(),
                sellUnit.getId(),
                soldQty,
                deductedBaseQty,
                afterStock
        );
    }

    /**
     * Executes the bulkAdjustStock operation.
     *
     * @param ids Parameter of type {@code List<Long>} used by this operation.
     * @param operation Parameter of type {@code String} used by this operation.
     * @param qty Parameter of type {@code String} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public int bulkAdjustStock(List<Long> ids, String operation, String qty) {
        if (ids == null || ids.isEmpty()) {
            throw new IllegalArgumentException("Select at least one product for bulk update.");
        }
        Integer qtyValue = parseInteger(qty);
        if (qtyValue == null || qtyValue <= 0) {
            throw new IllegalArgumentException("Bulk quantity must be a positive number.");
        }
        int delta = "remove".equalsIgnoreCase(operation) ? -qtyValue : qtyValue;
        String reference = "bulk-" + System.currentTimeMillis();
        List<Map<String, Object>> before = new ArrayList<>();
        List<Map<String, Object>> after = new ArrayList<>();
        int updatedCount = 0;
        for (Long id : ids) {
            Product existing = productRepo.findById(id).orElse(null);
            if (existing == null) continue;
            Map<String, Object> beforeRow = new LinkedHashMap<>();
            beforeRow.put("id", existing.getId());
            beforeRow.put("stockQty", existing.getStockQty());
            before.add(beforeRow);

            Product updated = stockMovementService.adjustByDelta(
                    existing.getId(),
                    delta,
                    existing.getCostPrice(),
                    null,
                    StockMovementType.ADJUSTMENT,
                    "ADJ_BULK",
                    reference,
                    null,
                    "Bulk stock " + (delta < 0 ? "remove" : "add")
            );

            Map<String, Object> afterRow = new LinkedHashMap<>();
            afterRow.put("id", updated.getId());
            afterRow.put("stockQty", updated.getStockQty());
            after.add(afterRow);
            updatedCount++;
        }
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("operation", operation);
        metadata.put("qty", qtyValue);
        metadata.put("delta", delta);
        metadata.put("affectedProducts", updatedCount);
        metadata.put("reference", reference);
        auditEventService.record("STOCK_BULK_UPDATE", "PRODUCT", "bulk", before, after, metadata);
        return updatedCount;
    }

    /**
     * Executes the setStockFromAdjustment operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param targetStock Parameter of type {@code int} used by this operation.
     * @param reference Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Product setStockFromAdjustment(Long productId,
                                          int targetStock,
                                          String reference,
                                          String notes) {
        return syncStockLevel(productId, targetStock, StockMovementType.ADJUSTMENT, reference, notes);
    }

    /**
     * Executes the setStockFromImport operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param targetStock Parameter of type {@code int} used by this operation.
     * @param reference Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Product setStockFromImport(Long productId,
                                      int targetStock,
                                      String reference,
                                      String notes) {
        return syncStockLevel(productId, targetStock, StockMovementType.IMPORT, reference, notes);
    }

    /**
     * Executes the syncStockLevel operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param targetStock Parameter of type {@code int} used by this operation.
     * @param movementType Parameter of type {@code StockMovementType} used by this operation.
     * @param reference Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Product syncStockLevel(Long productId,
                                   int targetStock,
                                   StockMovementType movementType,
                                   String reference,
                                   String notes) {
        return stockMovementService.adjustToTarget(
                productId,
                targetStock,
                null,
                null,
                movementType,
                movementType == StockMovementType.IMPORT ? "IMPORT" : "ADJ",
                reference,
                null,
                notes
        );
    }

    /**
     * Executes the recordImportSummary operation.
     *
     * @param filename Parameter of type {@code String} used by this operation.
     * @param allowCreate Parameter of type {@code boolean} used by this operation.
     * @param createCategories Parameter of type {@code boolean} used by this operation.
     * @param created Parameter of type {@code int} used by this operation.
     * @param updated Parameter of type {@code int} used by this operation.
     * @param skipped Parameter of type {@code int} used by this operation.
     * @param failed Parameter of type {@code int} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void recordImportSummary(String filename,
                                    boolean allowCreate,
                                    boolean createCategories,
                                    int created,
                                    int updated,
                                    int skipped,
                                    int failed) {
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("filename", filename);
        metadata.put("allowCreate", allowCreate);
        metadata.put("createCategories", createCategories);
        metadata.put("created", created);
        metadata.put("updated", updated);
        metadata.put("skipped", skipped);
        metadata.put("failed", failed);
        auditEventService.record("STOCK_IMPORT", "PRODUCT", "import", null, null, metadata);
    }

    /**
     * Executes the productSnapshot operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, Object> productSnapshot(Product product) {
        Map<String, Object> snapshot = new LinkedHashMap<>();
        snapshot.put("id", product.getId());
        snapshot.put("name", product.getName());
        snapshot.put("sku", product.getSku());
        snapshot.put("price", product.getPrice());
        snapshot.put("stockQty", product.getStockQty());
        snapshot.put("active", product.getActive());
        return snapshot;
    }

    /**
     * Executes the parseInteger operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code Integer} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Integer parseInteger(String value) {
        if (!hasText(value)) return null;
        try {
            return Integer.valueOf(value.trim());
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    /**
     * Executes the parseBigDecimal operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal parseBigDecimal(String value) {
        if (!hasText(value)) return null;
        try {
            return new BigDecimal(value.trim());
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    /**
     * Executes the hasText operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean hasText(String value) {
        return value != null && !value.trim().isEmpty();
    }

    /**
     * Executes the safeStock operation.
     *
     * @param value Parameter of type {@code Integer} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int safeStock(Integer value) {
        return value == null ? 0 : value;
    }

    /**
     * Executes the normalizeQty operation.
     *
     * @param qty Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal normalizeQty(BigDecimal qty) {
        if (qty == null) return BigDecimal.ZERO.setScale(6, RoundingMode.HALF_UP);
        return qty.setScale(6, RoundingMode.HALF_UP);
    }

    /**
     * Executes the variantSnapshot operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param stockBaseUnits Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, Object> variantSnapshot(Long variantId, BigDecimal stockBaseUnits) {
        Map<String, Object> snapshot = new LinkedHashMap<>();
        snapshot.put("variantId", variantId);
        snapshot.put("stockBaseUnits", stockBaseUnits);
        return snapshot;
    }

    public record VariantUnitDeductionResult(Long variantId,
                                             Long sellUnitId,
                                             BigDecimal soldQty,
                                             BigDecimal deductedBaseQty,
                                             BigDecimal remainingBaseQty) {
    }
}
