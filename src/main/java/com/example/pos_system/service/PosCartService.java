package com.example.pos_system.service;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.dto.CartItem;
import com.example.pos_system.entity.DiscountType;
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
