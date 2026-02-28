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

import java.time.LocalDateTime;

@Getter
@Setter
@Entity
@Table(name = "product_variant_exclusion", uniqueConstraints = {
        @UniqueConstraint(name = "uk_product_variant_exclusion", columnNames = {"product_id", "combination_hash"})
}, indexes = {
        @Index(name = "idx_product_variant_exclusion_product_active", columnList = "product_id,active")
})
public class ProductVariantExclusion {
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

    @Column(length = 255)
    private String reason;

    @Column(nullable = false)
    private Boolean active = true;

    @Column(name = "created_by", length = 128)
    private String createdBy;

    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt = LocalDateTime.now();
}
