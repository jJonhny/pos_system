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
import jakarta.persistence.Version;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;

@Getter
@Setter
@Entity
@Table(name = "sku_sell_unit", uniqueConstraints = {
        @UniqueConstraint(name = "uk_sku_sell_unit", columnNames = {"variant_id", "unit_id"})
}, indexes = {
        @Index(name = "idx_sku_sell_unit_variant", columnList = "variant_id"),
        @Index(name = "idx_sku_sell_unit_variant_enabled", columnList = "variant_id,enabled")
})
public class SkuSellUnit {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "variant_id", nullable = false)
    private ProductVariant variant;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "unit_id", nullable = false)
    private UnitOfMeasure unit;

    @Column(name = "conversion_to_base", nullable = false, precision = 19, scale = 6)
    private BigDecimal conversionToBase;

    @Column(name = "is_base", nullable = false)
    private Boolean isBase = false;

    @Column(name = "base_price", nullable = false, precision = 19, scale = 4)
    private BigDecimal basePrice;

    @Column(nullable = false)
    private Boolean enabled = true;

    @Version
    @Column(nullable = false)
    private Long version = 0L;
}
