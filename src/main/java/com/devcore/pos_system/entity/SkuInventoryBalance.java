package com.devcore.pos_system.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.MapsId;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Setter
@Entity
@Table(name = "sku_inventory_balance", indexes = {
        @Index(name = "idx_sku_inventory_balance_updated", columnList = "updated_at")
})
public class SkuInventoryBalance {
    @Id
    @Column(name = "variant_id")
    private Long variantId;

    @MapsId
    @OneToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "variant_id", nullable = false)
    private ProductVariant variant;

    @Column(name = "on_hand_base_qty", nullable = false, precision = 19, scale = 6)
    private BigDecimal onHandBaseQty = BigDecimal.ZERO;

    @Column(name = "reserved_base_qty", nullable = false, precision = 19, scale = 6)
    private BigDecimal reservedBaseQty = BigDecimal.ZERO;

    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt = LocalDateTime.now();
}
