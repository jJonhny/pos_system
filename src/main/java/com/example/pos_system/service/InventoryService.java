package com.example.pos_system.service;

import com.example.pos_system.entity.Product;
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

    public InventoryService(ProductRepo productRepo, AuditEventService auditEventService) {
        this.productRepo = productRepo;
        this.auditEventService = auditEventService;
    }

    public Product quickUpdate(Long id, String price, String stockQty) {
        Product product = productRepo.findByIdForUpdate(id).orElseThrow();
        Map<String, Object> before = productSnapshot(product);
        boolean changedPrice = false;
        boolean changedStock = false;

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
            Integer parsedStock = parseInteger(stockQty);
            if (parsedStock == null || parsedStock < 0) {
                throw new IllegalArgumentException("Invalid stock quantity.");
            }
            Integer current = product.getStockQty();
            if (current == null || current.intValue() != parsedStock) {
                product.setStockQty(parsedStock);
                changedStock = true;
            }
        }

        if (!changedPrice && !changedStock) {
            throw new IllegalArgumentException("No changes provided for quick update.");
        }

        Product saved = productRepo.save(product);
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
        List<Product> products = new ArrayList<>();
        for (Long id : ids) {
            productRepo.findByIdForUpdate(id).ifPresent(products::add);
        }
        List<Map<String, Object>> before = new ArrayList<>();
        List<Map<String, Object>> after = new ArrayList<>();

        for (Product product : products) {
            Map<String, Object> beforeRow = new LinkedHashMap<>();
            beforeRow.put("id", product.getId());
            beforeRow.put("stockQty", product.getStockQty());
            before.add(beforeRow);

            Integer current = product.getStockQty();
            int next = (current == null ? 0 : current) + delta;
            if (next < 0) next = 0;
            product.setStockQty(next);

            Map<String, Object> afterRow = new LinkedHashMap<>();
            afterRow.put("id", product.getId());
            afterRow.put("stockQty", product.getStockQty());
            after.add(afterRow);
        }
        productRepo.saveAll(products);
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("operation", operation);
        metadata.put("qty", qtyValue);
        metadata.put("delta", delta);
        metadata.put("affectedProducts", products.size());
        auditEventService.record("STOCK_BULK_UPDATE", "PRODUCT", "bulk", before, after, metadata);
        return products.size();
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
}
