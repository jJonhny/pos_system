package com.example.pos_system.service;

import com.example.pos_system.entity.Product;
import com.example.pos_system.repository.ProductRepo;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

@Service
@Transactional(readOnly = true)
public class ProductFeedService {
    private static final int DEFAULT_BATCH_SIZE = 24;
    private static final int MAX_BATCH_SIZE = 60;
    private static final int MIN_BATCH_SIZE = 8;

    private final ProductRepo productRepo;
    private final CursorTokenService cursorTokenService;

    /**
     * Executes the ProductFeedService operation.
     * <p>Return value: A fully initialized ProductFeedService instance.</p>
     *
     * @param productRepo Parameter of type {@code ProductRepo} used by this operation.
     * @param cursorTokenService Parameter of type {@code CursorTokenService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ProductFeedService(ProductRepo productRepo, CursorTokenService cursorTokenService) {
        this.productRepo = productRepo;
        this.cursorTokenService = cursorTokenService;
    }

    /**
     * Executes the fetchFeed operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param cursor Parameter of type {@code String} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @return {@code ProductFeedSlice} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ProductFeedSlice fetchFeed(String q,
                                      Long categoryId,
                                      String cursor,
                                      Integer size) {
        String normalizedQuery = normalizeQuery(q);
        int batchSize = clampSize(size);
        Long cursorId = cursorTokenService.parseProductCursor(cursor, normalizedQuery, categoryId);

        Pageable pageable = PageRequest.of(0, batchSize + 1);
        List<Product> rows = productRepo.findActiveFeedBatch(normalizedQuery, categoryId, cursorId, pageable);

        boolean hasMore = rows.size() > batchSize;
        List<Product> items;
        if (hasMore) {
            items = new ArrayList<>(rows.subList(0, batchSize));
        } else {
            items = new ArrayList<>(rows);
        }

        String nextCursor = null;
        if (hasMore && !items.isEmpty()) {
            Long lastId = items.getLast().getId();
            if (lastId != null) {
                nextCursor = cursorTokenService.createProductCursor(lastId, normalizedQuery, categoryId);
            }
        }

        return new ProductFeedSlice(items, nextCursor, hasMore, batchSize, normalizedQuery, categoryId);
    }

    /**
     * Executes the toFeedItem operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code ProductFeedItem} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ProductFeedItem toFeedItem(Product product) {
        if (product == null) return null;
        Long category = product.getCategory() == null ? null : product.getCategory().getId();
        String categoryName = product.getCategory() == null ? null : product.getCategory().getName();
        return new ProductFeedItem(
                product.getId(),
                product.getName(),
                product.getSku(),
                product.getBarcode(),
                product.getPrice(),
                product.getWholesalePrice(),
                product.getWholesaleMinQty(),
                product.getStockQty(),
                product.getLowStockThreshold(),
                product.getUnitsPerBox(),
                product.getUnitsPerCase(),
                product.getImageUrl(),
                Boolean.TRUE.equals(product.getActive()),
                product.isLowStock(),
                category,
                categoryName
        );
    }

    /**
     * Executes the clampSize operation.
     *
     * @param size Parameter of type {@code Integer} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int clampSize(Integer size) {
        if (size == null) return DEFAULT_BATCH_SIZE;
        int safe = Math.max(MIN_BATCH_SIZE, size);
        return Math.min(MAX_BATCH_SIZE, safe);
    }

    /**
     * Executes the normalizeQuery operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalizeQuery(String value) {
        if (value == null) return null;
        String trimmed = value.trim();
        if (trimmed.isEmpty()) return null;
        return trimmed.toLowerCase();
    }

    public record ProductFeedSlice(
            List<Product> items,
            String nextCursor,
            boolean hasMore,
            int batchSize,
            String normalizedQuery,
            Long categoryId
    ) {
    }

    public record ProductFeedItem(
            Long id,
            String name,
            String sku,
            String barcode,
            BigDecimal price,
            BigDecimal wholesalePrice,
            Integer wholesaleMinQty,
            Integer stockQty,
            Integer lowStockThreshold,
            Integer unitsPerBox,
            Integer unitsPerCase,
            String imageUrl,
            boolean active,
            boolean lowStock,
            Long categoryId,
            String categoryName
    ) {
    }
}
