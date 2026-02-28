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
import java.time.LocalDateTime;

@Getter
@Setter
@Entity
@Table(name = "product_variant", uniqueConstraints = {
        @UniqueConstraint(name = "uk_product_variant_combination", columnNames = {"product_id", "combination_hash"}),
        @UniqueConstraint(name = "uk_product_variant_sku", columnNames = {"sku"}),
        @UniqueConstraint(name = "uk_product_variant_barcode", columnNames = {"barcode"})
}, indexes = {
        @Index(name = "idx_product_variant_product", columnList = "product_id"),
        @Index(name = "idx_product_variant_product_enabled", columnList = "product_id,enabled,archived")
})
public class ProductVariant {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "product_id", nullable = false)
    private Product product;

    @Column(name = "combination_key", nullable = false, length = 1024)
    private String combinationKey;

    @Column(name = "combination_hash", nullable = false, length = 64)
    private String combinationHash;

    @Column(name = "variant_name", length = 255)
    private String variantName;

    @Column(length = 128)
    private String sku;

    @Column(length = 128)
    private String barcode;

    @Column(precision = 19, scale = 4)
    private BigDecimal price;

    @Column(precision = 19, scale = 4)
    private BigDecimal cost;

    @Column(name = "stock_base_qty", precision = 19, scale = 6, nullable = false)
    private BigDecimal stockBaseQty = BigDecimal.ZERO;

    @Column(nullable = false)
    private Boolean enabled = true;

    @Column(nullable = false)
    private Boolean impossible = false;

    @Column(nullable = false)
    private Boolean archived = false;

    @Version
    @Column(nullable = false)
    private Long version = 0L;

    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt = LocalDateTime.now();

    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt = LocalDateTime.now();
}
