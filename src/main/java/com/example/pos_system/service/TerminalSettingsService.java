package com.example.pos_system.service;

import com.example.pos_system.entity.PrinterMode;
import com.example.pos_system.entity.TerminalSettings;
import com.example.pos_system.repository.TerminalSettingsRepo;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Locale;
import java.util.Optional;

@Service
@Transactional
public class TerminalSettingsService {
    public static final String DEFAULT_BRIDGE_URL = "http://127.0.0.1:18765";

    private final TerminalSettingsRepo terminalSettingsRepo;
    private final CurrencyService currencyService;
    private final AuditEventService auditEventService;

    public TerminalSettingsService(TerminalSettingsRepo terminalSettingsRepo,
                                   CurrencyService currencyService,
                                   AuditEventService auditEventService) {
        this.terminalSettingsRepo = terminalSettingsRepo;
        this.currencyService = currencyService;
        this.auditEventService = auditEventService;
    }

    @Transactional(readOnly = true)
    public List<TerminalSettings> list() {
        return terminalSettingsRepo.findAllByOrderByNameAscTerminalIdAsc();
    }

    @Transactional(readOnly = true)
    public Optional<TerminalSettings> findById(Long id) {
        return terminalSettingsRepo.findById(id);
    }

    @Transactional(readOnly = true)
    public Optional<TerminalSettings> findByTerminalId(String terminalId) {
        String key = sanitizeTerminalId(terminalId);
        if (key == null) return Optional.empty();
        return terminalSettingsRepo.findByTerminalIdIgnoreCase(key);
    }

    @Transactional(readOnly = true)
    public String preferredTerminalId() {
        return list().stream()
                .findFirst()
                .map(TerminalSettings::getTerminalId)
                .orElse(null);
    }

    @Transactional(readOnly = true)
    public TerminalSettings resolveForTerminal(String terminalId) {
        String key = sanitizeTerminalId(terminalId);
        if (key != null) {
            Optional<TerminalSettings> byTerminal = terminalSettingsRepo.findByTerminalIdIgnoreCase(key);
            if (byTerminal.isPresent()) return byTerminal.get();
        }
        return list().stream().findFirst().orElseGet(() -> buildDefaultSettings(key));
    }

    public TerminalSettings save(Long id,
                                 String terminalId,
                                 String name,
                                 String defaultCurrency,
                                 String receiptHeader,
                                 String receiptFooter,
                                 String taxId,
                                 PrinterMode printerMode,
                                 String bridgeUrl,
                                 Boolean autoPrintEnabled,
                                 Boolean cameraScannerEnabled) {
        TerminalSettings existing = id == null ? null : terminalSettingsRepo.findById(id).orElse(null);
        TerminalSettings settings = existing == null ? new TerminalSettings() : existing;

        String normalizedTerminalId = sanitizeTerminalId(terminalId);
        if (normalizedTerminalId == null) {
            throw new IllegalArgumentException("Terminal ID is required.");
        }
        String normalizedName = trimTo(name, 120);
        if (normalizedName == null) {
            throw new IllegalArgumentException("Terminal name is required.");
        }

        terminalSettingsRepo.findByTerminalIdIgnoreCase(normalizedTerminalId)
                .filter(found -> settings.getId() == null || !found.getId().equals(settings.getId()))
                .ifPresent(found -> {
                    throw new IllegalArgumentException("Terminal ID already exists.");
                });

        var before = existing == null ? null : snapshot(existing);

        settings.setTerminalId(normalizedTerminalId);
        settings.setName(normalizedName);
        settings.setDefaultCurrency(normalizeCurrency(defaultCurrency));
        settings.setReceiptHeader(trimTo(receiptHeader, 255));
        settings.setReceiptFooter(trimTo(receiptFooter, 500));
        settings.setTaxId(trimTo(taxId, 64));
        settings.setPrinterMode(printerMode == null ? PrinterMode.PDF : printerMode);
        settings.setBridgeUrl(normalizeBridgeUrl(bridgeUrl));
        settings.setAutoPrintEnabled(Boolean.TRUE.equals(autoPrintEnabled));
        settings.setCameraScannerEnabled(Boolean.TRUE.equals(cameraScannerEnabled));

        if (settings.getCreatedAt() == null) {
            settings.setCreatedAt(LocalDateTime.now());
        }
        settings.setUpdatedAt(LocalDateTime.now());

        TerminalSettings saved = terminalSettingsRepo.save(settings);
        auditEventService.record(
                existing == null ? "TERMINAL_SETTINGS_CREATE" : "TERMINAL_SETTINGS_UPDATE",
                "TERMINAL_SETTINGS",
                saved.getId(),
                before,
                snapshot(saved),
                null
        );
        return saved;
    }

    public void delete(Long id) {
        if (id == null) return;
        TerminalSettings existing = terminalSettingsRepo.findById(id).orElse(null);
        if (existing == null) return;
        terminalSettingsRepo.delete(existing);
        auditEventService.record("TERMINAL_SETTINGS_DELETE", "TERMINAL_SETTINGS", id, snapshot(existing), null, null);
    }

    public String effectiveBridgeUrl(TerminalSettings settings) {
        if (settings == null) return DEFAULT_BRIDGE_URL;
        String configured = normalizeBridgeUrl(settings.getBridgeUrl());
        return configured == null ? DEFAULT_BRIDGE_URL : configured;
    }

    private TerminalSettings buildDefaultSettings(String terminalId) {
        TerminalSettings settings = new TerminalSettings();
        settings.setId(null);
        settings.setTerminalId(terminalId == null ? "TERM-DEFAULT" : terminalId);
        settings.setName("Default terminal");
        settings.setDefaultCurrency(currencyService.getBaseCurrency().getCode());
        settings.setReceiptHeader("Thank you for shopping with us");
        settings.setReceiptFooter("Please keep this receipt.");
        settings.setTaxId(null);
        settings.setPrinterMode(PrinterMode.PDF);
        settings.setBridgeUrl(DEFAULT_BRIDGE_URL);
        settings.setAutoPrintEnabled(false);
        settings.setCameraScannerEnabled(false);
        settings.setCreatedAt(LocalDateTime.now());
        settings.setUpdatedAt(LocalDateTime.now());
        return settings;
    }

    private String sanitizeTerminalId(String value) {
        String trimmed = trimTo(value, 128);
        if (trimmed == null) return null;
        return trimmed.toUpperCase(Locale.ROOT);
    }

    private String normalizeCurrency(String value) {
        String cleaned = trimTo(value, 8);
        if (cleaned == null) {
            return currencyService.getBaseCurrency().getCode();
        }
        return cleaned.toUpperCase(Locale.ROOT);
    }

    private String normalizeBridgeUrl(String value) {
        String cleaned = trimTo(value, 255);
        if (cleaned == null) return null;
        if (!cleaned.startsWith("http://") && !cleaned.startsWith("https://")) {
            return null;
        }
        return cleaned;
    }

    private String trimTo(String value, int maxLength) {
        if (value == null) return null;
        String trimmed = value.trim();
        if (trimmed.isEmpty()) return null;
        return trimmed.length() <= maxLength ? trimmed : trimmed.substring(0, maxLength);
    }

    private java.util.Map<String, Object> snapshot(TerminalSettings settings) {
        if (settings == null) return null;
        java.util.Map<String, Object> snapshot = new java.util.LinkedHashMap<>();
        snapshot.put("id", settings.getId());
        snapshot.put("terminalId", settings.getTerminalId());
        snapshot.put("name", settings.getName());
        snapshot.put("defaultCurrency", settings.getDefaultCurrency());
        snapshot.put("receiptHeader", settings.getReceiptHeader());
        snapshot.put("receiptFooter", settings.getReceiptFooter());
        snapshot.put("taxId", settings.getTaxId());
        snapshot.put("printerMode", settings.getPrinterMode() == null ? null : settings.getPrinterMode().name());
        snapshot.put("bridgeUrl", settings.getBridgeUrl());
        snapshot.put("autoPrintEnabled", settings.getAutoPrintEnabled());
        snapshot.put("cameraScannerEnabled", settings.getCameraScannerEnabled());
        snapshot.put("createdAt", settings.getCreatedAt());
        snapshot.put("updatedAt", settings.getUpdatedAt());
        return snapshot;
    }
}
