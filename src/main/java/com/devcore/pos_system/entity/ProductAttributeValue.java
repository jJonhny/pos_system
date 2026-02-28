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
@Table(name = "product_attribute_value", uniqueConstraints = {
        @UniqueConstraint(name = "uk_product_attribute_value", columnNames = {"product_id", "group_id", "value_id"}),
        @UniqueConstraint(name = "uk_product_attribute_value_value", columnNames = {"product_id", "value_id"})
}, indexes = {
        @Index(name = "idx_product_attribute_value_product_group", columnList = "product_id,group_id")
})
public class ProductAttributeValue {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "product_id", nullable = false)
    private Product product;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "group_id", nullable = false)
    private AttributeGroup group;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "value_id", nullable = false)
    private AttributeValue value;

    @Column(nullable = false)
    private Boolean active = true;
}
