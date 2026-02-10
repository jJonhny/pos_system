package com.example.pos_system.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;

@Getter
@Setter
@Entity
public class SaleItem {
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    private Sale sale;

    @ManyToOne(fetch = FetchType.LAZY)
    private Product product;

    private Integer qty;
    private BigDecimal unitPrice;
    private BigDecimal lineTotal;

    @Enumerated(EnumType.STRING)
    @Column(length = 16)
    private PriceTier priceTier;

    @Enumerated(EnumType.STRING)
    @Column(length = 16)
    private UnitType unitType;

    private Integer unitSize;

    @Column(length = 255)
    private String note;

    private Integer returnedQty;
}
