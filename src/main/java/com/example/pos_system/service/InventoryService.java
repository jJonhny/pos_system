package com.example.pos_system.service;

import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.StockMovementType;
import com.example.pos_system.repository.ProductRepo;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
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

    public InventoryService(ProductRepo productRepo, AuditEventService auditEventService,
                            StockMovementService stockMovementService) {
        this.productRepo = productRepo;
        this.auditEventService = auditEventService;
        this.stockMovementService = stockMovementService;
    }

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

    public Product setStockFromAdjustment(Long productId,
                                          int targetStock,
                                          String reference,
                                          String notes) {
        return syncStockLevel(productId, targetStock, StockMovementType.ADJUSTMENT, reference, notes);
    }

    public Product setStockFromImport(Long productId,
                                      int targetStock,
                                      String reference,
                                      String notes) {
        return syncStockLevel(productId, targetStock, StockMovementType.IMPORT, reference, notes);
    }

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

    private Integer parseInteger(String value) {
        if (!hasText(value)) return null;
        try {
            return Integer.valueOf(value.trim());
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    private BigDecimal parseBigDecimal(String value) {
        if (!hasText(value)) return null;
        try {
            return new BigDecimal(value.trim());
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    private boolean hasText(String value) {
        return value != null && !value.trim().isEmpty();
    }

    private int safeStock(Integer value) {
        return value == null ? 0 : value;
    }
}
