package com.example.pos_system.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@Entity
@Table(indexes = {
        @Index(name = "idx_sale_shift", columnList = "shift_id")
})
public class Sale {
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private LocalDateTime createdAt;

    private BigDecimal subtotal;
    private BigDecimal discount;
    @Enumerated(EnumType.STRING)
    @Column(length = 16)
    private DiscountType discountType;
    private BigDecimal discountValue;
    @Column(length = 255)
    private String discountReason;
    @Column(length = 100)
    private String discountAppliedBy;
    private BigDecimal tax;
    private BigDecimal total;

    @Column(length = 100)
    private String cashierUsername;

    @Column(length = 128)
    private String terminalId;

    @Enumerated(EnumType.STRING)
    private PaymentMethod paymentMethod;

    @Column(length = 10)
    private String receiptLocale;

    @Enumerated(EnumType.STRING)
    @Column(length = 32)
    private SaleStatus status;

    @ManyToOne(fetch = FetchType.LAZY)
    private Customer customer;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "shift_id")
    private Shift shift;

    private Integer pointsEarned;

    private BigDecimal refundedTotal;

    @OneToMany(mappedBy = "sale", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<SaleItem> items = new ArrayList<>();

    @OneToMany(mappedBy = "sale", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<SalePayment> payments = new ArrayList<>();
}
