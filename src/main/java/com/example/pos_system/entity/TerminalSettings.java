package com.example.pos_system.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.PrePersist;
import jakarta.persistence.PreUpdate;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

@Getter
@Setter
@Entity
@Table(name = "terminal_settings", indexes = {
        @Index(name = "idx_terminal_settings_terminal", columnList = "terminal_id", unique = true)
})
public class TerminalSettings {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "terminal_id", length = 128, nullable = false, unique = true)
    private String terminalId;

    @Column(length = 120, nullable = false)
    private String name;

    @Column(length = 8)
    private String defaultCurrency;

    @Column(length = 255)
    private String receiptHeader;

    @Column(length = 500)
    private String receiptFooter;

    @Column(length = 64)
    private String taxId;

    @Enumerated(EnumType.STRING)
    @Column(length = 16, nullable = false)
    private PrinterMode printerMode = PrinterMode.PDF;

    @Column(length = 255)
    private String bridgeUrl;

    @Column(nullable = false)
    private Boolean autoPrintEnabled = Boolean.FALSE;

    @Column(nullable = false)
    private Boolean cameraScannerEnabled = Boolean.FALSE;

    @Column(nullable = false)
    private LocalDateTime createdAt;

    @Column(nullable = false)
    private LocalDateTime updatedAt;

    @PrePersist
    void onCreate() {
        LocalDateTime now = LocalDateTime.now();
        if (createdAt == null) createdAt = now;
        if (updatedAt == null) updatedAt = now;
        if (printerMode == null) printerMode = PrinterMode.PDF;
        if (autoPrintEnabled == null) autoPrintEnabled = Boolean.FALSE;
        if (cameraScannerEnabled == null) cameraScannerEnabled = Boolean.FALSE;
    }

    @PreUpdate
    void onUpdate() {
        updatedAt = LocalDateTime.now();
        if (printerMode == null) printerMode = PrinterMode.PDF;
        if (autoPrintEnabled == null) autoPrintEnabled = Boolean.FALSE;
        if (cameraScannerEnabled == null) cameraScannerEnabled = Boolean.FALSE;
    }
}
