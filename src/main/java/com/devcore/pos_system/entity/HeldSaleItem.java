package com.devcore.pos_system.entity;

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
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

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
    @NotFound(action = NotFoundAction.IGNORE)
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

    @Column(name = "variant_id")
    private Long variantId;

    @Column(name = "unit_id")
    private Long unitId;

    @Column(name = "qty_base")
    private Integer qtyBase;

    @Column(name = "sell_unit_id")
    private Long sellUnitId;

    @Column(name = "sell_unit_code", length = 64)
    private String sellUnitCode;

    @Column(name = "conversion_to_base", precision = 19, scale = 6)
    private BigDecimal conversionToBase;

    @Column(name = "price_source", length = 32)
    private String priceSource;

    @Column(name = "applied_tier_min_qty", precision = 19, scale = 6)
    private BigDecimal appliedTierMinQty;

    @Column(name = "applied_tier_group_code", length = 64)
    private String appliedTierGroupCode;

    @Column(length = 255)
    private String note;
}
