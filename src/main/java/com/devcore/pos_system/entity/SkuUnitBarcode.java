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

@Getter
@Setter
@Entity
@Table(name = "sku_unit_barcode", uniqueConstraints = {
        @UniqueConstraint(name = "uk_sku_unit_barcode", columnNames = {"barcode"})
}, indexes = {
        @Index(name = "idx_sku_unit_barcode_sell_unit", columnList = "sku_sell_unit_id")
})
public class SkuUnitBarcode {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "sku_sell_unit_id", nullable = false)
    private SkuSellUnit skuSellUnit;

    @Column(nullable = false, length = 128)
    private String barcode;

    @Column(name = "is_primary", nullable = false)
    private Boolean isPrimary = true;

    @Column(nullable = false)
    private Boolean active = true;
}
