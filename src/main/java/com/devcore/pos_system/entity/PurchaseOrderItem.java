package com.devcore.pos_system.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import java.math.BigDecimal;

@Getter
@Setter
@Entity
@Table(name = "purchase_order_item")
public class PurchaseOrderItem {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "po_id", nullable = false)
    private PurchaseOrder purchaseOrder;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_id", nullable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    private Product product;

    @Column(nullable = false)
    private Integer orderedQty;

    @Column(nullable = false)
    private Integer receivedQty;

    @Column(name = "unit_id")
    private Long unitId;

    @Column(name = "ordered_qty_base")
    private Integer orderedQtyBase;

    @Column(name = "received_qty_base")
    private Integer receivedQtyBase;

    @Column(nullable = false, precision = 18, scale = 4)
    private BigDecimal unitCost;

    @Column(precision = 18, scale = 4)
    private BigDecimal tax;

    @Column(precision = 18, scale = 4)
    private BigDecimal discount;
}
