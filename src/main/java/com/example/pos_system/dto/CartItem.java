package com.example.pos_system.dto;

import com.example.pos_system.entity.PriceTier;
import com.example.pos_system.entity.UnitType;

import java.math.BigDecimal;

public class CartItem {
    private Long productId;
    private String name;
    private BigDecimal unitPrice;
    private int qty;
    private String note;
    private PriceTier priceTier = PriceTier.RETAIL;
    private UnitType unitType = UnitType.PIECE;
    private Integer unitSize = 1;

    public CartItem() {}

    public CartItem(Long productId, String name, BigDecimal unitPrice, int qty) {
        this(productId, name, unitPrice, qty, PriceTier.RETAIL, UnitType.PIECE, 1);
    }

    public CartItem(Long productId, String name, BigDecimal unitPrice, int qty, PriceTier priceTier,
                    UnitType unitType, int unitSize) {
        this.productId = productId;
        this.name = name;
        this.unitPrice = unitPrice;
        this.qty = qty;
        this.priceTier = priceTier == null ? PriceTier.RETAIL : priceTier;
        this.unitType = unitType == null ? UnitType.PIECE : unitType;
        this.unitSize = unitSize <= 0 ? 1 : unitSize;
    }

    public BigDecimal getLineTotal() {
        if (unitPrice == null) return BigDecimal.ZERO;
        return unitPrice.multiply(BigDecimal.valueOf(qty));
    }

    public Long getProductId() { return productId; }
    public void setProductId(Long productId) { this.productId = productId; }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public BigDecimal getUnitPrice() { return unitPrice; }
    public void setUnitPrice(BigDecimal unitPrice) { this.unitPrice = unitPrice; }

    public int getQty() { return qty; }
    public void setQty(int qty) { this.qty = qty; }

    public String getNote() { return note; }
    public void setNote(String note) { this.note = note; }

    public PriceTier getPriceTier() {
        return priceTier == null ? PriceTier.RETAIL : priceTier;
    }
    public void setPriceTier(PriceTier priceTier) {
        this.priceTier = priceTier == null ? PriceTier.RETAIL : priceTier;
    }

    public UnitType getUnitType() {
        return unitType == null ? UnitType.PIECE : unitType;
    }
    public void setUnitType(UnitType unitType) {
        this.unitType = unitType == null ? UnitType.PIECE : unitType;
    }

    public int getUnitSize() {
        return unitSize == null || unitSize <= 0 ? 1 : unitSize;
    }
    public void setUnitSize(Integer unitSize) {
        this.unitSize = unitSize == null || unitSize <= 0 ? 1 : unitSize;
    }

    public int getEffectiveQty() {
        return getUnitSize() * qty;
    }
}
