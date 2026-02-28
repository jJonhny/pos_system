package com.devcore.pos_system.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Setter
@Entity
@Table(name = "sku_unit_tier_price", uniqueConstraints = {
        @UniqueConstraint(name = "uk_sku_unit_tier_price", columnNames = {
                "sku_sell_unit_id", "customer_group_id", "min_qty", "effective_from"
        })
}, indexes = {
        @Index(name = "idx_sku_unit_tier_lookup", columnList = "sku_sell_unit_id,customer_group_id,min_qty")
})
public class SkuUnitTierPrice {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "sku_sell_unit_id", nullable = false)
    private SkuSellUnit skuSellUnit;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "customer_group_id")
    private CustomerGroup customerGroup;

    @Column(name = "min_qty", nullable = false, precision = 19, scale = 6)
    private BigDecimal minQty;

    @Column(name = "unit_price", nullable = false, precision = 19, scale = 4)
    private BigDecimal unitPrice;

    @Column(name = "currency_code", nullable = false, length = 8)
    private String currencyCode;

    @Column(name = "effective_from")
    private LocalDateTime effectiveFrom;

    @Column(name = "effective_to")
    private LocalDateTime effectiveTo;

    @Column(nullable = false)
    private Boolean active = true;
}
