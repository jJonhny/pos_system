package com.example.pos_system.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;

@Getter
@Setter
@Entity
public class Product {
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(unique = true)
    private String sku;

    @Column(unique = true)
    private String barcode;

    private String name;
    private BigDecimal price;
    private BigDecimal wholesalePrice;
    private Integer wholesaleMinQty;
    private BigDecimal costPrice;
    private Integer stockQty;
    private Integer lowStockThreshold;
    private Integer unitsPerBox;
    private Integer unitsPerCase;
    private Boolean active = true;
    @Column(length = 2048)
    private String imageUrl;

    @ManyToOne(fetch = FetchType.LAZY)
    private Category category;

    @Transient
    public boolean isLowStock() {
        if (stockQty == null || lowStockThreshold == null) return false;
        return stockQty <= lowStockThreshold;
    }
}
