package com.example.pos_system.dto;

import com.example.pos_system.entity.DiscountType;
import com.example.pos_system.entity.PriceTier;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.UnitType;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;

public class Cart {
    private final Map<Long, CartItem> items = new LinkedHashMap<>();
    private DiscountType discountType = DiscountType.AMOUNT;
    private BigDecimal discountValue = BigDecimal.ZERO;
    private String discountReason;
    private boolean manualDiscountOverride;
    private BigDecimal taxRate = new BigDecimal("0.00");
    private Long customerId;

    /**
     * Executes the add operation.
     *
     * @param p Parameter of type {@code Product} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void add(Product p) {
        add(p, PriceTier.RETAIL, UnitType.PIECE, 1, p.getPrice());
    }

    /**
     * Executes the add operation.
     *
     * @param p Parameter of type {@code Product} used by this operation.
     * @param priceTier Parameter of type {@code PriceTier} used by this operation.
     * @param unitType Parameter of type {@code UnitType} used by this operation.
     * @param unitSize Parameter of type {@code int} used by this operation.
     * @param unitPrice Parameter of type {@code BigDecimal} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void add(Product p, PriceTier priceTier, UnitType unitType, int unitSize, BigDecimal unitPrice) {
        items.compute(p.getId(), (k, v) -> {
            if (v == null) return new CartItem(p.getId(), p.getName(), unitPrice, 1, priceTier, unitType, unitSize);
            v.setQty(v.getQty() + 1);
            return v;
        });
    }

    /**
     * Executes the addVariant operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param sellUnitCode Parameter of type {@code String} used by this operation.
     * @param conversionToBase Parameter of type {@code BigDecimal} used by this operation.
     * @param unitPrice Parameter of type {@code BigDecimal} used by this operation.
     * @param qtyToAdd Parameter of type {@code int} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void addVariant(Long productId,
                           Long variantId,
                           Long sellUnitId,
                           String name,
                           String sellUnitCode,
                           BigDecimal conversionToBase,
                           BigDecimal unitPrice,
                           int qtyToAdd) {
        if (productId == null || variantId == null || sellUnitId == null) {
            throw new IllegalArgumentException("productId, variantId, and sellUnitId are required.");
        }
        int safeQty = qtyToAdd <= 0 ? 1 : qtyToAdd;
        items.compute(productId, (k, v) -> {
            if (v == null) {
                CartItem item = new CartItem(productId, name, unitPrice, safeQty, PriceTier.RETAIL, UnitType.PIECE, 1);
                item.setVariantId(variantId);
                item.setSellUnitId(sellUnitId);
                item.setSellUnitCode(sellUnitCode);
                item.setConversionToBase(conversionToBase);
                return item;
            }
            if (!sameVariant(v, variantId, sellUnitId)) {
                throw new IllegalStateException("Another variant of this product is already in the cart.");
            }
            v.setQty(v.getQty() + safeQty);
            if (name != null && !name.isBlank()) v.setName(name);
            if (unitPrice != null) v.setUnitPrice(unitPrice);
            if (sellUnitCode != null && !sellUnitCode.isBlank()) v.setSellUnitCode(sellUnitCode);
            if (conversionToBase != null) v.setConversionToBase(conversionToBase);
            return v;
        });
    }

    /**
     * Executes the setQty operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param qty Parameter of type {@code int} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setQty(Long productId, int qty) {
        if (qty <= 0) items.remove(productId);
        else if (items.containsKey(productId)) items.get(productId).setQty(qty);
    }

    /**
     * Executes the remove operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void remove(Long productId) { items.remove(productId); }

    /**
     * Executes the getItems operation.
     *
     * @return {@code Collection<CartItem>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Collection<CartItem> getItems() { return items.values(); }

    /**
     * Executes the addItem operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param unitPrice Parameter of type {@code BigDecimal} used by this operation.
     * @param qty Parameter of type {@code int} used by this operation.
     * @param note Parameter of type {@code String} used by this operation.
     * @param priceTier Parameter of type {@code PriceTier} used by this operation.
     * @param unitType Parameter of type {@code UnitType} used by this operation.
     * @param unitSize Parameter of type {@code int} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void addItem(Long productId, String name, BigDecimal unitPrice, int qty, String note, PriceTier priceTier,
                        UnitType unitType, int unitSize) {
        addItem(productId, name, unitPrice, qty, note, priceTier, unitType, unitSize,
                null, null, null, null, null, null, null);
    }

    /**
     * Executes the addItem operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param unitPrice Parameter of type {@code BigDecimal} used by this operation.
     * @param qty Parameter of type {@code int} used by this operation.
     * @param note Parameter of type {@code String} used by this operation.
     * @param priceTier Parameter of type {@code PriceTier} used by this operation.
     * @param unitType Parameter of type {@code UnitType} used by this operation.
     * @param unitSize Parameter of type {@code int} used by this operation.
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @param sellUnitCode Parameter of type {@code String} used by this operation.
     * @param conversionToBase Parameter of type {@code BigDecimal} used by this operation.
     * @param priceSource Parameter of type {@code String} used by this operation.
     * @param appliedTierMinQty Parameter of type {@code BigDecimal} used by this operation.
     * @param appliedTierGroupCode Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void addItem(Long productId, String name, BigDecimal unitPrice, int qty, String note, PriceTier priceTier,
                        UnitType unitType, int unitSize, Long variantId, Long sellUnitId, String sellUnitCode,
                        BigDecimal conversionToBase, String priceSource, BigDecimal appliedTierMinQty,
                        String appliedTierGroupCode) {
        CartItem item = new CartItem(productId, name, unitPrice, qty, priceTier, unitType, unitSize);
        item.setNote(note);
        item.setVariantId(variantId);
        item.setSellUnitId(sellUnitId);
        item.setSellUnitCode(sellUnitCode);
        item.setConversionToBase(conversionToBase);
        item.setPriceSource(priceSource);
        item.setAppliedTierMinQty(appliedTierMinQty);
        item.setAppliedTierGroupCode(appliedTierGroupCode);
        items.put(productId, item);
    }

    /**
     * Executes the setPriceTier operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param priceTier Parameter of type {@code PriceTier} used by this operation.
     * @param unitPrice Parameter of type {@code BigDecimal} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setPriceTier(Long productId, PriceTier priceTier, BigDecimal unitPrice) {
        CartItem item = items.get(productId);
        if (item == null) return;
        item.setPriceTier(priceTier);
        item.setUnitPrice(unitPrice);
    }

    /**
     * Executes the setUnit operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param unitType Parameter of type {@code UnitType} used by this operation.
     * @param unitSize Parameter of type {@code int} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setUnit(Long productId, UnitType unitType, int unitSize) {
        CartItem item = items.get(productId);
        if (item == null) return;
        item.setUnitType(unitType);
        item.setUnitSize(unitSize);
    }

    /**
     * Executes the getItem operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @return {@code CartItem} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public CartItem getItem(Long productId) { return items.get(productId); }

    /**
     * Executes the hasVariantConflict operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public boolean hasVariantConflict(Long productId, Long variantId, Long sellUnitId) {
        CartItem existing = items.get(productId);
        if (existing == null) return false;
        if (existing.getVariantId() == null || existing.getSellUnitId() == null) return variantId != null || sellUnitId != null;
        return !sameVariant(existing, variantId, sellUnitId);
    }

    /**
     * Executes the clear operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void clear() {
        items.clear();
        discountType = DiscountType.AMOUNT;
        discountValue = BigDecimal.ZERO;
        discountReason = null;
        manualDiscountOverride = false;
        taxRate = new BigDecimal("0.00");
        customerId = null;
    }

    /**
     * Executes the setNote operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param note Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setNote(Long productId, String note) {
        CartItem item = items.get(productId);
        if (item == null) return;
        String cleaned = note == null ? null : note.trim();
        item.setNote(cleaned == null || cleaned.isEmpty() ? null : cleaned);
    }

    /**
     * Executes the getSubtotal operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getSubtotal() {
        return items.values().stream()
                .map(CartItem::getLineTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    /**
     * Executes the getDiscountType operation.
     *
     * @return {@code DiscountType} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public DiscountType getDiscountType() {
        return discountType == null ? DiscountType.AMOUNT : discountType;
    }
    /**
     * Executes the setDiscountType operation.
     *
     * @param discountType Parameter of type {@code DiscountType} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setDiscountType(DiscountType discountType) {
        this.discountType = discountType == null ? DiscountType.AMOUNT : discountType;
    }

    /**
     * Executes the getDiscountValue operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getDiscountValue() {
        return discountValue == null ? BigDecimal.ZERO : discountValue;
    }
    /**
     * Executes the setDiscountValue operation.
     *
     * @param discountValue Parameter of type {@code BigDecimal} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setDiscountValue(BigDecimal discountValue) {
        this.discountValue = discountValue == null ? BigDecimal.ZERO : discountValue;
    }

    /**
     * Executes the getDiscountReason operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String getDiscountReason() { return discountReason; }
    /**
     * Executes the setDiscountReason operation.
     *
     * @param discountReason Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setDiscountReason(String discountReason) {
        String cleaned = discountReason == null ? null : discountReason.trim();
        this.discountReason = cleaned == null || cleaned.isEmpty() ? null : cleaned;
    }

    /**
     * Executes the isManualDiscountOverride operation.
     *
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public boolean isManualDiscountOverride() { return manualDiscountOverride; }
    /**
     * Executes the setManualDiscountOverride operation.
     *
     * @param manualDiscountOverride Parameter of type {@code boolean} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setManualDiscountOverride(boolean manualDiscountOverride) {
        this.manualDiscountOverride = manualDiscountOverride;
    }

    /**
     * Executes the getDiscount operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getDiscount() {
        BigDecimal subtotal = getSubtotal();
        if (subtotal.compareTo(BigDecimal.ZERO) <= 0) return BigDecimal.ZERO;
        BigDecimal amount;
        if (getDiscountType() == DiscountType.PERCENT) {
            BigDecimal pct = clamp(getDiscountValue(), BigDecimal.ZERO, new BigDecimal("100"));
            amount = subtotal.multiply(pct).divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP);
        } else {
            amount = getDiscountValue();
        }
        if (amount.compareTo(subtotal) > 0) amount = subtotal;
        return amount.max(BigDecimal.ZERO);
    }

    /**
     * Executes the setDiscount operation.
     *
     * @param discount Parameter of type {@code BigDecimal} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setDiscount(BigDecimal discount) {
        setDiscountType(DiscountType.AMOUNT);
        setDiscountValue(discount == null ? BigDecimal.ZERO : discount.max(BigDecimal.ZERO));
    }

    /**
     * Executes the getTaxRate operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getTaxRate() { return taxRate; }
    /**
     * Executes the setTaxRate operation.
     *
     * @param taxRate Parameter of type {@code BigDecimal} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setTaxRate(BigDecimal taxRate) { this.taxRate = taxRate; }

    /**
     * Executes the getCustomerId operation.
     *
     * @return {@code Long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Long getCustomerId() { return customerId; }
    /**
     * Executes the setCustomerId operation.
     *
     * @param customerId Parameter of type {@code Long} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setCustomerId(Long customerId) { this.customerId = customerId; }

    /**
     * Executes the getTaxRatePercent operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getTaxRatePercent() {
        return taxRate.multiply(new BigDecimal("100"));
    }

    /**
     * Executes the getTax operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getTax() {
        return effectiveSubtotal().multiply(taxRate);
    }

    /**
     * Executes the getTotal operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getTotal() {
        return effectiveSubtotal().add(getTax());
    }

    /**
     * Executes the effectiveSubtotal operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal effectiveSubtotal() {
        BigDecimal subtotal = getSubtotal();
        BigDecimal result = subtotal.subtract(getDiscount());
        return result.compareTo(BigDecimal.ZERO) < 0 ? BigDecimal.ZERO : result;
    }

    /**
     * Executes the clamp operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @param min Parameter of type {@code BigDecimal} used by this operation.
     * @param max Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal clamp(BigDecimal value, BigDecimal min, BigDecimal max) {
        BigDecimal safe = value == null ? BigDecimal.ZERO : value;
        if (min != null && safe.compareTo(min) < 0) return min;
        if (max != null && safe.compareTo(max) > 0) return max;
        return safe;
    }

    /**
     * Executes the sameVariant operation.
     *
     * @param item Parameter of type {@code CartItem} used by this operation.
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean sameVariant(CartItem item, Long variantId, Long sellUnitId) {
        if (item == null) return false;
        return Objects.equals(item.getVariantId(), variantId)
                && Objects.equals(item.getSellUnitId(), sellUnitId);
    }
}
