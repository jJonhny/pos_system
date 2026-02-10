package com.example.pos_system.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.JoinColumn;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;

@Getter
@Setter
@Entity
public class HeldSaleItem {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    private HeldSale heldSale;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_id")
    private Product product;

    @Column(name = "product_ref_id")
    private Long productId;

    private String name;
    private BigDecimal unitPrice;
    private Integer qty;

    @Enumerated(EnumType.STRING)
    @Column(length = 16)
    private PriceTier priceTier;

    @Enumerated(EnumType.STRING)
    @Column(length = 16)
    private UnitType unitType;

    private Integer unitSize;

    @Column(length = 255)
    private String note;
}
