package com.devcore.pos_system.entity;

import jakarta.persistence.*;
import jakarta.persistence.EntityNotFoundException;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;
import org.hibernate.ObjectNotFoundException;

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
    @JoinColumn(name = "product_id")
    @NotFound(action = NotFoundAction.IGNORE)
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

    private Integer returnedQty;

    @Transient
    public String getSafeProductName() {
        try {
            if (product == null) return null;
            String name = product.getName();
            if (name == null || name.isBlank()) return null;
            return name;
        } catch (EntityNotFoundException | ObjectNotFoundException ex) {
            return null;
        }
    }
}
