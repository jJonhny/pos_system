package com.example.pos_system.service;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.dto.CartItem;
import com.example.pos_system.entity.DiscountType;
import com.example.pos_system.entity.HeldSale;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.LinkedHashMap;
import java.util.Map;

@Service
public class PosCartService {
    private final AuditEventService auditEventService;

    public PosCartService(AuditEventService auditEventService) {
        this.auditEventService = auditEventService;
    }

    @Transactional
    public void applyDiscount(Cart cart, DiscountType discountType, BigDecimal discountValue, String discountReason) {
        Map<String, Object> before = cartSnapshot(cart);
        DiscountType safeType = discountType == null ? DiscountType.AMOUNT : discountType;
        BigDecimal safeValue = discountValue == null ? BigDecimal.ZERO : discountValue.max(BigDecimal.ZERO);
        if (safeType == DiscountType.PERCENT && safeValue.compareTo(new BigDecimal("100")) > 0) {
            safeValue = new BigDecimal("100");
        }
        if (safeType == DiscountType.AMOUNT) {
            BigDecimal subtotal = cart.getSubtotal();
            if (subtotal != null && safeValue.compareTo(subtotal) > 0) {
                safeValue = subtotal;
            }
        }
        cart.setDiscountType(safeType);
        cart.setDiscountValue(safeValue);
        cart.setDiscountReason(discountReason);
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("discountType", safeType.name());
        metadata.put("discountValue", safeValue);
        metadata.put("discountReason", cart.getDiscountReason());
        auditEventService.record("POS_CART_DISCOUNT", "CART", "session", before, cartSnapshot(cart), metadata);
    }

    @Transactional
    public void applyTax(Cart cart, BigDecimal taxRatePercent) {
        Map<String, Object> before = cartSnapshot(cart);
        BigDecimal safeRatePercent = taxRatePercent == null ? BigDecimal.ZERO : taxRatePercent.max(BigDecimal.ZERO);
        if (safeRatePercent.compareTo(new BigDecimal("100")) > 0) {
            safeRatePercent = new BigDecimal("100");
        }
        BigDecimal rate = safeRatePercent.divide(new BigDecimal("100"), 4, java.math.RoundingMode.HALF_UP);
        cart.setTaxRate(rate);
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("taxRatePercent", safeRatePercent);
        metadata.put("taxRate", rate);
        auditEventService.record("POS_CART_TAX_OVERRIDE", "CART", "session", before, cartSnapshot(cart), metadata);
    }

    @Transactional
    public void recordPriceOverride(Cart cart, CartItem before, CartItem after, String reason) {
        if (before == null || after == null) return;
        if (!hasPriceChange(before, after)) return;
        Map<String, Object> beforeState = lineSnapshot(before);
        Map<String, Object> afterState = lineSnapshot(after);
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("reason", reason);
        metadata.put("cartTotal", cart.getTotal());
        auditEventService.record("POS_CART_PRICE_OVERRIDE", "CART_ITEM", after.getProductId(),
                beforeState, afterState, metadata);
    }

    @Transactional
    public void recordHoldCart(Cart cart, HeldSale hold) {
        if (cart == null || hold == null) return;
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("holdId", hold.getId());
        metadata.put("label", hold.getLabel());
        metadata.put("itemCount", hold.getItems() == null ? 0 : hold.getItems().size());
        metadata.put("customerId", hold.getCustomer() == null ? null : hold.getCustomer().getId());
        auditEventService.record("POS_HOLD_CART", "HOLD", hold.getId(), cartSnapshot(cart), null, metadata);
    }

    @Transactional
    public void recordResumeHold(HeldSale hold, Cart cart) {
        if (hold == null || cart == null) return;
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("label", hold.getLabel());
        metadata.put("itemCount", hold.getItems() == null ? 0 : hold.getItems().size());
        metadata.put("customerId", hold.getCustomer() == null ? null : hold.getCustomer().getId());
        auditEventService.record("POS_RESUME_HOLD", "HOLD", hold.getId(), null, cartSnapshot(cart), metadata);
    }

    private boolean hasPriceChange(CartItem before, CartItem after) {
        if (before.getUnitPrice() == null && after.getUnitPrice() != null) return true;
        if (before.getUnitPrice() != null && after.getUnitPrice() == null) return true;
        if (before.getUnitPrice() != null && after.getUnitPrice() != null
                && before.getUnitPrice().compareTo(after.getUnitPrice()) != 0) {
            return true;
        }
        if (before.getPriceTier() != after.getPriceTier()) return true;
        return before.getUnitType() != after.getUnitType() || before.getUnitSize() != after.getUnitSize();
    }

    private Map<String, Object> cartSnapshot(Cart cart) {
        Map<String, Object> snapshot = new LinkedHashMap<>();
        snapshot.put("subtotal", cart.getSubtotal());
        snapshot.put("discountType", cart.getDiscountType() == null ? null : cart.getDiscountType().name());
        snapshot.put("discountValue", cart.getDiscountValue());
        snapshot.put("discount", cart.getDiscount());
        snapshot.put("discountReason", cart.getDiscountReason());
        snapshot.put("taxRate", cart.getTaxRate());
        snapshot.put("taxRatePercent", cart.getTaxRatePercent());
        snapshot.put("tax", cart.getTax());
        snapshot.put("total", cart.getTotal());
        snapshot.put("itemCount", cart.getItems().size());
        return snapshot;
    }

    private Map<String, Object> lineSnapshot(CartItem item) {
        Map<String, Object> snapshot = new LinkedHashMap<>();
        snapshot.put("productId", item.getProductId());
        snapshot.put("name", item.getName());
        snapshot.put("qty", item.getQty());
        snapshot.put("unitPrice", item.getUnitPrice());
        snapshot.put("priceTier", item.getPriceTier() == null ? null : item.getPriceTier().name());
        snapshot.put("unitType", item.getUnitType() == null ? null : item.getUnitType().name());
        snapshot.put("unitSize", item.getUnitSize());
        snapshot.put("lineTotal", item.getLineTotal());
        return snapshot;
    }
}
