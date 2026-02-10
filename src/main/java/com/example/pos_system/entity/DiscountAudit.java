package com.example.pos_system.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Setter
@Entity
public class DiscountAudit {
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private LocalDateTime createdAt;

    @ManyToOne(fetch = FetchType.LAZY)
    private Sale sale;

    @Enumerated(EnumType.STRING)
    @Column(length = 16)
    private DiscountType discountType;

    private BigDecimal discountValue;
    private BigDecimal discountAmount;
    private BigDecimal subtotalBefore;
    private BigDecimal subtotalAfter;

    @Column(length = 255)
    private String reason;

    @Column(length = 100)
    private String appliedBy;

    @Column(length = 20)
    private String scope;
}
