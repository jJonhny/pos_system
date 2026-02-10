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
    private BigDecimal taxRate = new BigDecimal("0.00");
    private Long customerId;

    public void add(Product p) {
        add(p, PriceTier.RETAIL, UnitType.PIECE, 1, p.getPrice());
    }

    public void add(Product p, PriceTier priceTier, UnitType unitType, int unitSize, BigDecimal unitPrice) {
        items.compute(p.getId(), (k, v) -> {
            if (v == null) return new CartItem(p.getId(), p.getName(), unitPrice, 1, priceTier, unitType, unitSize);
            v.setQty(v.getQty() + 1);
            return v;
        });
    }

    public void setQty(Long productId, int qty) {
        if (qty <= 0) items.remove(productId);
        else if (items.containsKey(productId)) items.get(productId).setQty(qty);
    }

    public void remove(Long productId) { items.remove(productId); }

    public Collection<CartItem> getItems() { return items.values(); }

    public void addItem(Long productId, String name, BigDecimal unitPrice, int qty, String note, PriceTier priceTier,
                        UnitType unitType, int unitSize) {
        CartItem item = new CartItem(productId, name, unitPrice, qty, priceTier, unitType, unitSize);
        item.setNote(note);
        items.put(productId, item);
    }

    public void setPriceTier(Long productId, PriceTier priceTier, BigDecimal unitPrice) {
        CartItem item = items.get(productId);
        if (item == null) return;
        item.setPriceTier(priceTier);
        item.setUnitPrice(unitPrice);
    }

    public void setUnit(Long productId, UnitType unitType, int unitSize) {
        CartItem item = items.get(productId);
        if (item == null) return;
        item.setUnitType(unitType);
        item.setUnitSize(unitSize);
    }

    public CartItem getItem(Long productId) { return items.get(productId); }

    public void clear() {
        items.clear();
        discountType = DiscountType.AMOUNT;
        discountValue = BigDecimal.ZERO;
        discountReason = null;
        taxRate = new BigDecimal("0.00");
        customerId = null;
    }

    public void setNote(Long productId, String note) {
        CartItem item = items.get(productId);
        if (item == null) return;
        String cleaned = note == null ? null : note.trim();
        item.setNote(cleaned == null || cleaned.isEmpty() ? null : cleaned);
    }

    public BigDecimal getSubtotal() {
        return items.values().stream()
                .map(CartItem::getLineTotal)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    public DiscountType getDiscountType() {
        return discountType == null ? DiscountType.AMOUNT : discountType;
    }
    public void setDiscountType(DiscountType discountType) {
        this.discountType = discountType == null ? DiscountType.AMOUNT : discountType;
    }

    public BigDecimal getDiscountValue() {
        return discountValue == null ? BigDecimal.ZERO : discountValue;
    }
    public void setDiscountValue(BigDecimal discountValue) {
        this.discountValue = discountValue == null ? BigDecimal.ZERO : discountValue;
    }

    public String getDiscountReason() { return discountReason; }
    public void setDiscountReason(String discountReason) {
        String cleaned = discountReason == null ? null : discountReason.trim();
        this.discountReason = cleaned == null || cleaned.isEmpty() ? null : cleaned;
    }

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

    public void setDiscount(BigDecimal discount) {
        setDiscountType(DiscountType.AMOUNT);
        setDiscountValue(discount == null ? BigDecimal.ZERO : discount.max(BigDecimal.ZERO));
    }

    public BigDecimal getTaxRate() { return taxRate; }
    public void setTaxRate(BigDecimal taxRate) { this.taxRate = taxRate; }

    public Long getCustomerId() { return customerId; }
    public void setCustomerId(Long customerId) { this.customerId = customerId; }

    public BigDecimal getTaxRatePercent() {
        return taxRate.multiply(new BigDecimal("100"));
    }

    public BigDecimal getTax() {
        return effectiveSubtotal().multiply(taxRate);
    }

    public BigDecimal getTotal() {
        return effectiveSubtotal().add(getTax());
    }

    private BigDecimal effectiveSubtotal() {
        BigDecimal subtotal = getSubtotal();
        BigDecimal result = subtotal.subtract(getDiscount());
        return result.compareTo(BigDecimal.ZERO) < 0 ? BigDecimal.ZERO : result;
    }

    private BigDecimal clamp(BigDecimal value, BigDecimal min, BigDecimal max) {
        BigDecimal safe = value == null ? BigDecimal.ZERO : value;
        if (min != null && safe.compareTo(min) < 0) return min;
        if (max != null && safe.compareTo(max) > 0) return max;
        return safe;
    }
}
