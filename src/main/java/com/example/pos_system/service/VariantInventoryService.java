package com.example.pos_system.service;

import com.example.pos_system.entity.ProductVariant;
import com.example.pos_system.entity.SkuInventoryBalance;
import com.example.pos_system.repository.ProductVariantRepo;
import com.example.pos_system.repository.SkuInventoryBalanceRepo;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;

@Service
@Transactional
public class VariantInventoryService {
    private final ProductVariantRepo productVariantRepo;
    private final SkuInventoryBalanceRepo skuInventoryBalanceRepo;

    /**
     * Executes the VariantInventoryService operation.
     * <p>Return value: A fully initialized VariantInventoryService instance.</p>
     *
     * @param productVariantRepo Parameter of type {@code ProductVariantRepo} used by this operation.
     * @param skuInventoryBalanceRepo Parameter of type {@code SkuInventoryBalanceRepo} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public VariantInventoryService(ProductVariantRepo productVariantRepo,
                                   SkuInventoryBalanceRepo skuInventoryBalanceRepo) {
        this.productVariantRepo = productVariantRepo;
        this.skuInventoryBalanceRepo = skuInventoryBalanceRepo;
    }

    /**
     * Executes the recordSale operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param baseQty Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code ProductVariant} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ProductVariant recordSale(Long variantId, BigDecimal baseQty) {
        if (variantId == null) {
            throw new IllegalArgumentException("Variant not found.");
        }
        BigDecimal delta = normalizeQty(baseQty);
        if (delta.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("Sold quantity must be greater than zero.");
        }

        ProductVariant variant = productVariantRepo.findByIdForUpdate(variantId)
                .orElseThrow(() -> new IllegalArgumentException("Variant not found."));
        if (Boolean.TRUE.equals(variant.getArchived())
                || Boolean.TRUE.equals(variant.getImpossible())
                || !Boolean.TRUE.equals(variant.getEnabled())) {
            throw new IllegalStateException("Variant is not saleable.");
        }
        if (variant.getProduct() == null || Boolean.FALSE.equals(variant.getProduct().getActive())) {
            throw new IllegalStateException("Product is inactive.");
        }

        BigDecimal current = normalizeQty(variant.getStockBaseQty());
        BigDecimal next = current.subtract(delta);
        boolean allowNegative = variant.getProduct() != null
                && Boolean.TRUE.equals(variant.getProduct().getAllowNegativeStock());
        if (!allowNegative && next.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalStateException("Insufficient variant stock for " + safeName(variant) + ".");
        }

        BigDecimal scaledNext = next.setScale(6, RoundingMode.HALF_UP);
        variant.setStockBaseQty(scaledNext);
        variant.setUpdatedAt(LocalDateTime.now());
        ProductVariant saved = productVariantRepo.save(variant);

        SkuInventoryBalance balance = skuInventoryBalanceRepo.findByVariantIdForUpdate(saved.getId())
                .orElseGet(() -> {
                    SkuInventoryBalance b = new SkuInventoryBalance();
                    b.setVariant(saved);
                    b.setReservedBaseQty(BigDecimal.ZERO.setScale(6, RoundingMode.HALF_UP));
                    return b;
                });
        balance.setVariant(saved);
        balance.setOnHandBaseQty(scaledNext);
        if (balance.getReservedBaseQty() == null) {
            balance.setReservedBaseQty(BigDecimal.ZERO.setScale(6, RoundingMode.HALF_UP));
        } else {
            balance.setReservedBaseQty(balance.getReservedBaseQty().setScale(6, RoundingMode.HALF_UP));
        }
        balance.setUpdatedAt(LocalDateTime.now());
        skuInventoryBalanceRepo.save(balance);

        return saved;
    }

    /**
     * Executes the normalizeQty operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal normalizeQty(BigDecimal value) {
        if (value == null) return BigDecimal.ZERO.setScale(6, RoundingMode.HALF_UP);
        return value.setScale(6, RoundingMode.HALF_UP);
    }

    /**
     * Executes the safeName operation.
     *
     * @param variant Parameter of type {@code ProductVariant} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String safeName(ProductVariant variant) {
        if (variant == null) return "variant";
        if (variant.getVariantName() != null && !variant.getVariantName().isBlank()) {
            return variant.getVariantName();
        }
        if (variant.getProduct() != null && variant.getProduct().getName() != null && !variant.getProduct().getName().isBlank()) {
            return variant.getProduct().getName();
        }
        return "variant #" + variant.getId();
    }
}
