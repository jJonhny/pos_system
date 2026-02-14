package com.example.pos_system.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Column;
import jakarta.persistence.Lob;
import jakarta.persistence.Index;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Setter
@Entity
@Table(name = "shift", indexes = {
        @Index(name = "idx_shift_terminal_status", columnList = "terminal_id,status"),
        @Index(name = "idx_shift_opened_at", columnList = "opened_at"),
        @Index(name = "idx_shift_closed_at", columnList = "closed_at")
})
public class Shift {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(length = 100)
    private String openedBy;

    private Long openedByUserId;

    @Column(length = 100)
    private String cashierUsername;

    private LocalDateTime openedAt;

    @Column(length = 100)
    private String closedBy;

    private Long closedByUserId;

    private LocalDateTime closedAt;

    private BigDecimal openingCash;
    private BigDecimal closingCash;

    private BigDecimal totalSales;
    private BigDecimal cashTotal;
    private BigDecimal cardTotal;
    private BigDecimal qrTotal;

    @Column(precision = 18, scale = 2)
    private BigDecimal cashInTotal;

    @Column(precision = 18, scale = 2)
    private BigDecimal cashOutTotal;

    @Column(precision = 18, scale = 2)
    private BigDecimal cashRefundTotal;

    @Column(precision = 18, scale = 2)
    private BigDecimal expectedCash;

    @Column(precision = 18, scale = 2)
    private BigDecimal varianceCash;

    @Column(length = 128)
    private String terminalId;

    @Column(length = 1000)
    private String closeNotes;

    @Lob
    @Column(columnDefinition = "TEXT")
    private String openingFloatJson;

    @Lob
    @Column(columnDefinition = "TEXT")
    private String countedAmountsJson;

    @Lob
    @Column(columnDefinition = "TEXT")
    private String expectedAmountsJson;

    @Lob
    @Column(columnDefinition = "TEXT")
    private String varianceAmountsJson;

    @Enumerated(EnumType.STRING)
    private ShiftStatus status;
}
