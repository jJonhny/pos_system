package com.example.pos_system.service;

import com.example.pos_system.entity.PrinterMode;
import com.example.pos_system.entity.TerminalSettings;
import com.example.pos_system.modules.currency.application.CurrencyService;
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

    /**
     * Executes the TerminalSettingsService operation.
     * <p>Return value: A fully initialized TerminalSettingsService instance.</p>
     *
     * @param terminalSettingsRepo Parameter of type {@code TerminalSettingsRepo} used by this operation.
     * @param currencyService Parameter of type {@code CurrencyService} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public TerminalSettingsService(TerminalSettingsRepo terminalSettingsRepo,
                                   CurrencyService currencyService,
                                   AuditEventService auditEventService) {
        this.terminalSettingsRepo = terminalSettingsRepo;
        this.currencyService = currencyService;
        this.auditEventService = auditEventService;
    }

    /**
     * Executes the list operation.
     *
     * @return {@code List<TerminalSettings>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the list operation.
     *
     * @return {@code List<TerminalSettings>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the list operation.
     *
     * @return {@code List<TerminalSettings>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public List<TerminalSettings> list() {
        return terminalSettingsRepo.findAllByOrderByNameAscTerminalIdAsc();
    }

    /**
     * Executes the findById operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<TerminalSettings>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findById operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<TerminalSettings>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findById operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<TerminalSettings>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public Optional<TerminalSettings> findById(Long id) {
        return terminalSettingsRepo.findById(id);
    }

    /**
     * Executes the findByTerminalId operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code Optional<TerminalSettings>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findByTerminalId operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code Optional<TerminalSettings>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findByTerminalId operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code Optional<TerminalSettings>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public Optional<TerminalSettings> findByTerminalId(String terminalId) {
        String key = sanitizeTerminalId(terminalId);
        if (key == null) return Optional.empty();
        return terminalSettingsRepo.findByTerminalIdIgnoreCase(key);
    }

    /**
     * Executes the preferredTerminalId operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the preferredTerminalId operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the preferredTerminalId operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public String preferredTerminalId() {
        return list().stream()
                .findFirst()
                .map(TerminalSettings::getTerminalId)
                .orElse(null);
    }

    /**
     * Executes the resolveForTerminal operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code TerminalSettings} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the resolveForTerminal operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code TerminalSettings} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the resolveForTerminal operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code TerminalSettings} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public TerminalSettings resolveForTerminal(String terminalId) {
        String key = sanitizeTerminalId(terminalId);
        if (key != null) {
            Optional<TerminalSettings> byTerminal = terminalSettingsRepo.findByTerminalIdIgnoreCase(key);
            if (byTerminal.isPresent()) return byTerminal.get();
        }
        return list().stream().findFirst().orElseGet(() -> buildDefaultSettings(key));
    }

    /**
     * Executes the save operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param defaultCurrency Parameter of type {@code String} used by this operation.
     * @param receiptHeader Parameter of type {@code String} used by this operation.
     * @param receiptFooter Parameter of type {@code String} used by this operation.
     * @param taxId Parameter of type {@code String} used by this operation.
     * @param printerMode Parameter of type {@code PrinterMode} used by this operation.
     * @param bridgeUrl Parameter of type {@code String} used by this operation.
     * @param autoPrintEnabled Parameter of type {@code Boolean} used by this operation.
     * @param cameraScannerEnabled Parameter of type {@code Boolean} used by this operation.
     * @return {@code TerminalSettings} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the delete operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void delete(Long id) {
        if (id == null) return;
        TerminalSettings existing = terminalSettingsRepo.findById(id).orElse(null);
        if (existing == null) return;
        terminalSettingsRepo.delete(existing);
        auditEventService.record("TERMINAL_SETTINGS_DELETE", "TERMINAL_SETTINGS", id, snapshot(existing), null, null);
    }

    /**
     * Executes the effectiveBridgeUrl operation.
     *
     * @param settings Parameter of type {@code TerminalSettings} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String effectiveBridgeUrl(TerminalSettings settings) {
        if (settings == null) return DEFAULT_BRIDGE_URL;
        String configured = normalizeBridgeUrl(settings.getBridgeUrl());
        return configured == null ? DEFAULT_BRIDGE_URL : configured;
    }

    /**
     * Executes the buildDefaultSettings operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code TerminalSettings} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the sanitizeTerminalId operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String sanitizeTerminalId(String value) {
        String trimmed = trimTo(value, 128);
        if (trimmed == null) return null;
        return trimmed.toUpperCase(Locale.ROOT);
    }

    /**
     * Executes the normalizeCurrency operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalizeCurrency(String value) {
        String cleaned = trimTo(value, 8);
        if (cleaned == null) {
            return currencyService.getBaseCurrency().getCode();
        }
        return cleaned.toUpperCase(Locale.ROOT);
    }

    /**
     * Executes the normalizeBridgeUrl operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalizeBridgeUrl(String value) {
        String cleaned = trimTo(value, 255);
        if (cleaned == null) return null;
        if (!cleaned.startsWith("http://") && !cleaned.startsWith("https://")) {
            return null;
        }
        return cleaned;
    }

    /**
     * Executes the trimTo operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @param maxLength Parameter of type {@code int} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String trimTo(String value, int maxLength) {
        if (value == null) return null;
        String trimmed = value.trim();
        if (trimmed.isEmpty()) return null;
        return trimmed.length() <= maxLength ? trimmed : trimmed.substring(0, maxLength);
    }

    /**
     * Executes the snapshot operation.
     *
     * @param settings Parameter of type {@code TerminalSettings} used by this operation.
     * @return {@code java.util.Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
