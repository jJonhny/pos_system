package com.devcore.pos_system.entity;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@Entity
public class HeldSale {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String label;
    private String cashierUsername;
    private LocalDateTime createdAt;

    private BigDecimal discount;
    @Enumerated(EnumType.STRING)
    @Column(length = 16)
    private DiscountType discountType;
    private BigDecimal discountValue;
    @Column(length = 255)
    private String discountReason;
    private BigDecimal taxRate;

    @ManyToOne(fetch = FetchType.LAZY)
    private Customer customer;

    @OneToMany(mappedBy = "heldSale", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<HeldSaleItem> items = new ArrayList<>();
}
