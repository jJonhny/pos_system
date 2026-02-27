package com.example.pos_system.service;

import com.example.pos_system.entity.PrinterMode;
import com.example.pos_system.entity.Shift;
import com.example.pos_system.entity.ShiftCashEventType;
import com.example.pos_system.entity.TerminalSettings;
import com.example.pos_system.repository.SaleRepo;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;

import java.time.LocalDateTime;
import java.util.LinkedHashMap;
import java.util.Map;

@Service
@Transactional
public class PosHardwareService {
    private final SaleRepo saleRepo;
    private final TerminalSettingsService terminalSettingsService;
    private final ReceiptPayloadService receiptPayloadService;
    private final ShiftService shiftService;
    private final AuditEventService auditEventService;

    /**
     * Executes the PosHardwareService operation.
     * <p>Return value: A fully initialized PosHardwareService instance.</p>
     *
     * @param saleRepo Parameter of type {@code SaleRepo} used by this operation.
     * @param terminalSettingsService Parameter of type {@code TerminalSettingsService} used by this operation.
     * @param receiptPayloadService Parameter of type {@code ReceiptPayloadService} used by this operation.
     * @param shiftService Parameter of type {@code ShiftService} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public PosHardwareService(SaleRepo saleRepo,
                              TerminalSettingsService terminalSettingsService,
                              ReceiptPayloadService receiptPayloadService,
                              ShiftService shiftService,
                              AuditEventService auditEventService) {
        this.saleRepo = saleRepo;
        this.terminalSettingsService = terminalSettingsService;
        this.receiptPayloadService = receiptPayloadService;
        this.shiftService = shiftService;
        this.auditEventService = auditEventService;
    }

    /**
     * Executes the buildReceiptPrintResponse operation.
     *
     * @param saleId Parameter of type {@code Long} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param reprint Parameter of type {@code boolean} used by this operation.
     * @return {@code PrintResponse} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public PrintResponse buildReceiptPrintResponse(Long saleId, String terminalId, boolean reprint) {
        var sale = saleRepo.findByIdForReceipt(saleId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Sale not found."));

        String resolvedTerminalId = resolveTerminalId(terminalId, sale.getTerminalId());
        TerminalSettings settings = terminalSettingsService.resolveForTerminal(resolvedTerminalId);
        resolvedTerminalId = resolveTerminalId(resolvedTerminalId, settings.getTerminalId());
        ReceiptPayloadService.ReceiptPrintPayload payload = receiptPayloadService.buildPrintPayload(
                sale,
                settings,
                resolvedTerminalId
        );

        PrinterMode mode = settings.getPrinterMode() == null ? PrinterMode.PDF : settings.getPrinterMode();
        String bridgeUrl = terminalSettingsService.effectiveBridgeUrl(settings);

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("saleId", sale.getId());
        metadata.put("terminalId", resolvedTerminalId);
        metadata.put("printerMode", mode.name());
        metadata.put("bridgeUrl", bridgeUrl);
        metadata.put("reprint", reprint);
        metadata.put("paymentCount", sale.getPayments() == null ? 0 : sale.getPayments().size());

        auditEventService.record("PRINT_RECEIPT", "SALE", sale.getId(), null, null, metadata);

        return new PrintResponse(
                sale.getId(),
                resolvedTerminalId,
                mode.name(),
                bridgeUrl,
                "/sales/" + sale.getId() + "/receipt",
                "/sales/" + sale.getId() + "/receipt/pdf",
                "If bridge is unavailable, use PDF download and open drawer manually.",
                payload,
                settings.getAutoPrintEnabled() != null && settings.getAutoPrintEnabled()
        );
    }

    /**
     * Executes the openDrawer operation.
     *
     * @param actorUsername Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param saleId Parameter of type {@code Long} used by this operation.
     * @return {@code DrawerResponse} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public DrawerResponse openDrawer(String actorUsername, String terminalId, Long saleId) {
        String resolvedTerminalId = resolveTerminalId(terminalId, null);
        TerminalSettings settings = terminalSettingsService.resolveForTerminal(resolvedTerminalId);
        resolvedTerminalId = resolveTerminalId(resolvedTerminalId, settings.getTerminalId());
        PrinterMode mode = settings.getPrinterMode() == null ? PrinterMode.PDF : settings.getPrinterMode();
        String bridgeUrl = terminalSettingsService.effectiveBridgeUrl(settings);

        Long shiftId = null;
        Shift openShift = shiftService.findOpenShift(actorUsername, resolvedTerminalId).orElse(null);
        if (openShift != null && openShift.getId() != null) {
            shiftId = openShift.getId();
            shiftService.addCashEvent(
                    actorUsername,
                    resolvedTerminalId,
                    ShiftCashEventType.DRAWER_OPEN,
                    null,
                    null,
                    saleId == null ? "Drawer opened" : "Drawer opened for sale #" + saleId
            );
        }

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("terminalId", resolvedTerminalId);
        metadata.put("shiftId", shiftId);
        metadata.put("saleId", saleId);
        metadata.put("printerMode", mode.name());
        metadata.put("bridgeUrl", bridgeUrl);
        auditEventService.record("OPEN_DRAWER", "TERMINAL", resolvedTerminalId, null, null, metadata);

        return new DrawerResponse(
                true,
                resolvedTerminalId,
                mode.name(),
                bridgeUrl,
                shiftId,
                "Drawer command queued. If bridge fails, open drawer manually."
        );
    }

    /**
     * Executes the buildPrinterTestResponse operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code PrintResponse} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public PrintResponse buildPrinterTestResponse(String terminalId) {
        String resolvedTerminalId = resolveTerminalId(terminalId, null);
        TerminalSettings settings = terminalSettingsService.resolveForTerminal(resolvedTerminalId);
        resolvedTerminalId = resolveTerminalId(resolvedTerminalId, settings.getTerminalId());

        String text = "POS PRINTER TEST\n"
                + "Terminal: " + resolvedTerminalId + "\n"
                + "Printed at: " + LocalDateTime.now() + "\n"
                + "------------------------------------------\n"
                + "If this prints correctly, bridge setup is OK.\n";

        ReceiptPayloadService.ReceiptPrintPayload payload = new ReceiptPayloadService.ReceiptPrintPayload(
                "test-print",
                null,
                resolvedTerminalId,
                text,
                "test|terminal=" + resolvedTerminalId,
                java.util.List.of(),
                java.util.Map.of(),
                java.util.List.of(),
                java.util.Map.of("terminalId", resolvedTerminalId),
                true,
                "CP437"
        );

        PrinterMode mode = settings.getPrinterMode() == null ? PrinterMode.PDF : settings.getPrinterMode();
        String bridgeUrl = terminalSettingsService.effectiveBridgeUrl(settings);

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("terminalId", resolvedTerminalId);
        metadata.put("printerMode", mode.name());
        metadata.put("bridgeUrl", bridgeUrl);
        auditEventService.record("PRINTER_TEST", "TERMINAL", resolvedTerminalId, null, null, metadata);

        return new PrintResponse(
                null,
                resolvedTerminalId,
                mode.name(),
                bridgeUrl,
                null,
                null,
                "Test print payload ready.",
                payload,
                false
        );
    }

    /**
     * Executes the resolveTerminalId operation.
     *
     * @param preferred Parameter of type {@code String} used by this operation.
     * @param fallback Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String resolveTerminalId(String preferred, String fallback) {
        String first = sanitize(preferred);
        if (first != null) return first;
        String second = sanitize(fallback);
        if (second != null) return second;
        String third = terminalSettingsService.preferredTerminalId();
        if (third != null && !third.isBlank()) return third;
        return "TERM-DEFAULT";
    }

    /**
     * Executes the sanitize operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String sanitize(String terminalId) {
        if (terminalId == null) return null;
        String cleaned = terminalId.trim();
        if (cleaned.isEmpty()) return null;
        return cleaned.length() <= 128 ? cleaned : cleaned.substring(0, 128);
    }

    public record PrintResponse(
            Long saleId,
            String terminalId,
            String printerMode,
            String bridgeUrl,
            String receiptUrl,
            String pdfUrl,
            String fallbackMessage,
            ReceiptPayloadService.ReceiptPrintPayload payload,
            boolean autoPrintEnabled
    ) {
    }

    public record DrawerResponse(
            boolean ok,
            String terminalId,
            String printerMode,
            String bridgeUrl,
            Long shiftId,
            String message
    ) {
    }
}
