package com.devcore.pos_system.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.PrePersist;
import jakarta.persistence.PreUpdate;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Setter
@Entity
@Table(name = "product_unit", uniqueConstraints = {
        @UniqueConstraint(name = "uk_product_unit_product_name", columnNames = {"product_id", "name"}),
        @UniqueConstraint(name = "uk_product_unit_product_barcode", columnNames = {"product_id", "barcode"})
})
public class ProductUnit {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "product_id", nullable = false)
    private Product product;

    @Column(nullable = false, length = 64)
    private String name;

    @Column(length = 32)
    private String abbreviation;

    @Column(name = "conversion_to_base", nullable = false, precision = 19, scale = 6)
    private BigDecimal conversionToBase;

    @Column(name = "allow_for_sale", nullable = false)
    private Boolean allowForSale = true;

    @Column(name = "allow_for_purchase", nullable = false)
    private Boolean allowForPurchase = true;

    @Column(name = "is_default_sale_unit", nullable = false)
    private Boolean isDefaultSaleUnit = false;

    @Column(name = "is_default_purchase_unit", nullable = false)
    private Boolean isDefaultPurchaseUnit = false;

    @Column(length = 128)
    private String barcode;

    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @PrePersist
    public void onCreate() {
        LocalDateTime now = LocalDateTime.now();
        if (createdAt == null) createdAt = now;
        if (updatedAt == null) updatedAt = now;
        normalize();
    }

    @PreUpdate
    public void onUpdate() {
        updatedAt = LocalDateTime.now();
        normalize();
    }

    private void normalize() {
        if (name != null) name = name.trim();
        if (abbreviation != null) {
            abbreviation = abbreviation.trim();
            if (abbreviation.isEmpty()) abbreviation = null;
        }
        if (barcode != null) {
            barcode = barcode.trim();
            if (barcode.isEmpty()) barcode = null;
        }
        if (conversionToBase == null || conversionToBase.compareTo(BigDecimal.ZERO) <= 0) {
            conversionToBase = BigDecimal.ONE;
        }
        if (allowForSale == null) allowForSale = true;
        if (allowForPurchase == null) allowForPurchase = true;
        if (isDefaultSaleUnit == null) isDefaultSaleUnit = false;
        if (isDefaultPurchaseUnit == null) isDefaultPurchaseUnit = false;
    }
}
