package com.example.pos_system.service;

import com.example.pos_system.entity.PaymentMethod;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SaleItem;
import com.example.pos_system.entity.TerminalSettings;
import com.example.pos_system.entity.UnitType;
import com.example.pos_system.modules.currency.application.CurrencyService;
import com.example.pos_system.modules.currency.domain.Currency;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

@Service
@Transactional(readOnly = true)
public class ReceiptPayloadService {
    private static final DateTimeFormatter DATE_TIME = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
    private static final int WIDTH = 42;

    private final CurrencyService currencyService;
    private final ReceiptPaymentService receiptPaymentService;
    private final I18nService i18nService;

    /**
     * Executes the ReceiptPayloadService operation.
     * <p>Return value: A fully initialized ReceiptPayloadService instance.</p>
     *
     * @param currencyService Parameter of type {@code CurrencyService} used by this operation.
     * @param receiptPaymentService Parameter of type {@code ReceiptPaymentService} used by this operation.
     * @param i18nService Parameter of type {@code I18nService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ReceiptPayloadService(CurrencyService currencyService,
                                 ReceiptPaymentService receiptPaymentService,
                                 I18nService i18nService) {
        this.currencyService = currencyService;
        this.receiptPaymentService = receiptPaymentService;
        this.i18nService = i18nService;
    }

    /**
     * Executes the buildPrintPayload operation.
     *
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @param settings Parameter of type {@code TerminalSettings} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code ReceiptPrintPayload} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ReceiptPrintPayload buildPrintPayload(Sale sale, TerminalSettings settings, String terminalId) {
        Currency baseCurrency = currencyService.getBaseCurrency();
        Locale receiptLocale = i18nService.parseOrDefault(sale == null ? null : sale.getReceiptLocale());
        String resolvedTerminal = terminalId != null && !terminalId.isBlank()
                ? terminalId
                : (settings == null ? null : settings.getTerminalId());
        if (resolvedTerminal == null || resolvedTerminal.isBlank()) {
            resolvedTerminal = "TERM-DEFAULT";
        }

        List<ReceiptPaymentService.ReceiptPaymentLine> paymentLines = receiptPaymentService.buildLines(sale);
        String receiptText = buildReceiptText(sale, paymentLines, settings, baseCurrency, resolvedTerminal, receiptLocale);
        String qrPayload = buildQrPayload(sale, resolvedTerminal);

        List<Map<String, Object>> lines = new ArrayList<>();
        if (sale.getItems() != null) {
            for (SaleItem item : sale.getItems()) {
                Map<String, Object> line = new LinkedHashMap<>();
                line.put("productId", item.getProduct() == null ? null : item.getProduct().getId());
                line.put("name", item.getProduct() == null ? msg(receiptLocale, "receipt.itemDefault") : item.getProduct().getName());
                line.put("qty", item.getQty());
                line.put("unit", unitLabel(item.getUnitType(), receiptLocale));
                line.put("unitPrice", item.getUnitPrice());
                line.put("lineTotal", item.getLineTotal());
                lines.add(line);
            }
        }

        List<Map<String, Object>> payments = new ArrayList<>();
        for (ReceiptPaymentService.ReceiptPaymentLine paymentLine : paymentLines) {
            Map<String, Object> value = new LinkedHashMap<>();
            value.put("method", paymentLine.method() == null ? null : paymentLine.method().name());
            value.put("amount", paymentLine.amountBase());
            value.put("currencyCode", paymentLine.currencyCode());
            value.put("currencyRate", paymentLine.currencyRate());
            value.put("foreignAmount", paymentLine.foreignAmount());
            value.put("cashReceivedBase", paymentLine.cashReceivedBase());
            value.put("cashReceivedForeign", paymentLine.cashReceivedForeign());
            value.put("cashChangeBase", paymentLine.cashChangeBase());
            value.put("cashChangeForeign", paymentLine.cashChangeForeign());
            value.put("displayCashChange", paymentLine.displayCashChange());
            payments.add(value);
        }

        Map<String, Object> totals = new LinkedHashMap<>();
        totals.put("subtotal", safeMoney(sale.getSubtotal()));
        totals.put("discount", safeMoney(sale.getDiscount()));
        totals.put("tax", safeMoney(sale.getTax()));
        totals.put("total", safeMoney(sale.getTotal()));

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("saleId", sale.getId());
        metadata.put("terminalId", resolvedTerminal);
        metadata.put("cashier", sale.getCashierUsername());
        metadata.put("createdAt", sale.getCreatedAt());
        metadata.put("locale", receiptLocale.toLanguageTag());

        return new ReceiptPrintPayload(
                "receipt",
                sale.getId(),
                resolvedTerminal,
                receiptText,
                qrPayload,
                lines,
                totals,
                payments,
                metadata,
                true,
                "CP437"
        );
    }

    /**
     * Executes the buildReceiptText operation.
     *
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @param paymentLines Parameter of type {@code List<ReceiptPaymentService.ReceiptPaymentLine>} used by this operation.
     * @param settings Parameter of type {@code TerminalSettings} used by this operation.
     * @param baseCurrency Parameter of type {@code Currency} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param locale Parameter of type {@code Locale} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String buildReceiptText(Sale sale,
                                    List<ReceiptPaymentService.ReceiptPaymentLine> paymentLines,
                                    TerminalSettings settings,
                                    Currency baseCurrency,
                                    String terminalId,
                                    Locale locale) {
        StringBuilder out = new StringBuilder();
        String header = settings == null ? null : trimTo(settings.getReceiptHeader(), 255);
        if (header != null) {
            out.append(center(header)).append('\n');
        } else {
            out.append(center(msg(locale, "receipt.storeName"))).append('\n');
        }

        out.append(center(msg(locale, "receipt.labelWithId", safeId(sale.getId())))).append('\n');
        out.append(center(sale.getCreatedAt() == null ? "-" : sale.getCreatedAt().format(DATE_TIME))).append('\n');
        out.append(repeat('-', WIDTH)).append('\n');
        out.append(twoCol(msg(locale, "shift.terminal"), terminalId)).append('\n');
        out.append(twoCol(msg(locale, "receipt.cashier"), safeText(sale.getCashierUsername(), "-"))).append('\n');
        if (settings != null && settings.getTaxId() != null && !settings.getTaxId().isBlank()) {
            out.append(twoCol(msg(locale, "receipt.taxId"), settings.getTaxId())).append('\n');
        }
        out.append(repeat('-', WIDTH)).append('\n');

        if (sale.getItems() != null) {
            for (SaleItem item : sale.getItems()) {
                String itemDefault = msg(locale, "receipt.itemDefault");
                String name = item.getProduct() == null ? itemDefault : safeText(item.getProduct().getName(), itemDefault);
                out.append(trimToWidth(name, WIDTH)).append('\n');
                String qty = (item.getQty() == null ? 0 : item.getQty()) + " " + unitLabel(item.getUnitType(), locale);
                String rate = money(item.getUnitPrice(), baseCurrency);
                String lineTotal = money(item.getLineTotal(), baseCurrency);
                out.append(twoCol(qty + " x " + rate, lineTotal)).append('\n');
            }
        }

        out.append(repeat('-', WIDTH)).append('\n');
        out.append(twoCol(msg(locale, "pos.subtotal"), money(sale.getSubtotal(), baseCurrency))).append('\n');
        out.append(twoCol(msg(locale, "pos.discount"), money(sale.getDiscount(), baseCurrency))).append('\n');
        out.append(twoCol(msg(locale, "pos.tax"), money(sale.getTax(), baseCurrency))).append('\n');
        out.append(twoCol(msg(locale, "receipt.totalUpper"), money(sale.getTotal(), baseCurrency))).append('\n');

        if (paymentLines != null && !paymentLines.isEmpty()) {
            out.append(repeat('-', WIDTH)).append('\n');
            for (ReceiptPaymentService.ReceiptPaymentLine paymentLine : paymentLines) {
                String method = paymentMethodLabel(paymentLine.method(), locale).toUpperCase(locale);
                String amount = money(paymentLine.amountBase(), baseCurrency);
                out.append(twoCol(method, amount)).append('\n');
                if (paymentLine.currencyCode() != null && paymentLine.foreignAmount() != null) {
                    out.append("  ")
                            .append(paymentLine.foreignAmount().setScale(2, RoundingMode.HALF_UP).toPlainString())
                            .append(" ")
                            .append(paymentLine.currencyCode().toUpperCase(Locale.ROOT))
                            .append('\n');
                }
                if (paymentLine.cashReceivedBase() != null) {
                    out.append(twoCol("  " + msg(locale, "receipt.cashReceived"), money(paymentLine.cashReceivedBase(), baseCurrency))).append('\n');
                }
                if (paymentLine.cashReceivedForeign() != null && paymentLine.currencyCode() != null) {
                    out.append("    ")
                            .append(paymentLine.cashReceivedForeign().setScale(2, RoundingMode.HALF_UP).toPlainString())
                            .append(" ")
                            .append(paymentLine.currencyCode().toUpperCase(Locale.ROOT))
                            .append('\n');
                }
                if (paymentLine.displayCashChange() && paymentLine.cashChangeBase() != null) {
                    out.append(twoCol("  " + msg(locale, "receipt.change"), money(paymentLine.cashChangeBase(), baseCurrency))).append('\n');
                    if (paymentLine.cashChangeForeign() != null && paymentLine.currencyCode() != null) {
                        out.append("    ")
                                .append(paymentLine.cashChangeForeign().setScale(2, RoundingMode.HALF_UP).toPlainString())
                                .append(" ")
                                .append(paymentLine.currencyCode().toUpperCase(Locale.ROOT))
                                .append('\n');
                    }
                }
            }
        }

        out.append(repeat('-', WIDTH)).append('\n');
        out.append("QR: ").append(buildQrPayload(sale, terminalId)).append('\n');
        String footer = settings == null ? null : trimTo(settings.getReceiptFooter(), 500);
        if (footer != null) {
            out.append(center(footer)).append('\n');
        } else {
            out.append(center(msg(locale, "receipt.thankYou"))).append('\n');
        }
        return out.toString();
    }

    /**
     * Executes the buildQrPayload operation.
     *
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String buildQrPayload(Sale sale, String terminalId) {
        String date = sale.getCreatedAt() == null ? "-" : sale.getCreatedAt().format(DATE_TIME);
        return "sale=" + safeId(sale.getId())
                + "|terminal=" + safeText(terminalId, "-")
                + "|total=" + safeMoney(sale.getTotal()).setScale(2, RoundingMode.HALF_UP)
                + "|at=" + date;
    }

    /**
     * Executes the twoCol operation.
     *
     * @param left Parameter of type {@code String} used by this operation.
     * @param right Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String twoCol(String left, String right) {
        String safeLeft = safeText(left, "");
        String safeRight = safeText(right, "");
        int space = Math.max(1, WIDTH - safeRight.length() - safeLeft.length());
        if (space == 1 && safeLeft.length() > WIDTH - safeRight.length() - 1) {
            safeLeft = trimToWidth(safeLeft, Math.max(1, WIDTH - safeRight.length() - 1));
        }
        return safeLeft + " ".repeat(Math.max(1, WIDTH - safeRight.length() - safeLeft.length())) + safeRight;
    }

    /**
     * Executes the center operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String center(String value) {
        String safe = trimToWidth(safeText(value, ""), WIDTH);
        int pad = Math.max(0, (WIDTH - safe.length()) / 2);
        return " ".repeat(pad) + safe;
    }

    /**
     * Executes the repeat operation.
     *
     * @param c Parameter of type {@code char} used by this operation.
     * @param count Parameter of type {@code int} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String repeat(char c, int count) {
        return String.valueOf(c).repeat(Math.max(0, count));
    }

    /**
     * Executes the trimToWidth operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @param width Parameter of type {@code int} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String trimToWidth(String value, int width) {
        if (value == null) return "";
        return value.length() <= width ? value : value.substring(0, width);
    }

    /**
     * Executes the money operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @param currency Parameter of type {@code Currency} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String money(BigDecimal value, Currency currency) {
        BigDecimal scaled = safeMoney(value).setScale(currencyDecimals(currency), RoundingMode.HALF_UP);
        String symbol = currency == null ? "$" : currency.getSymbol();
        String code = currency == null ? "USD" : currency.getCode();
        if (symbol != null && !symbol.isBlank()) {
            return symbol + scaled.toPlainString();
        }
        return scaled.toPlainString() + " " + (code == null ? "" : code);
    }

    /**
     * Executes the currencyDecimals operation.
     *
     * @param currency Parameter of type {@code Currency} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int currencyDecimals(Currency currency) {
        if (currency == null || currency.getFractionDigits() == null) return 2;
        return Math.max(0, currency.getFractionDigits());
    }

    /**
     * Executes the safeMoney operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeMoney(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    /**
     * Executes the safeText operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @param fallback Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String safeText(String value, String fallback) {
        if (value == null) return fallback;
        String trimmed = value.trim();
        return trimmed.isEmpty() ? fallback : trimmed;
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
     * Executes the safeId operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String safeId(Long id) {
        return id == null ? "0" : String.valueOf(id);
    }

    /**
     * Executes the unitLabel operation.
     *
     * @param unitType Parameter of type {@code UnitType} used by this operation.
     * @param locale Parameter of type {@code Locale} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String unitLabel(UnitType unitType, Locale locale) {
        if (unitType == null) return msg(locale, "common.unit.piece");
        return switch (unitType) {
            case BOX -> msg(locale, "common.unit.box");
            case CASE -> msg(locale, "common.unit.case");
            default -> msg(locale, "common.unit.piece");
        };
    }

    /**
     * Executes the paymentMethodLabel operation.
     *
     * @param method Parameter of type {@code PaymentMethod} used by this operation.
     * @param locale Parameter of type {@code Locale} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String paymentMethodLabel(PaymentMethod method, Locale locale) {
        if (method == null) return msg(locale, "payment.method.cash");
        return switch (method) {
            case CASH -> msg(locale, "payment.method.cash");
            case CARD -> msg(locale, "payment.method.card");
            case QR -> msg(locale, "payment.method.qr");
            case MIXED -> msg(locale, "payment.method.mixed");
        };
    }

    /**
     * Executes the msg operation.
     *
     * @param locale Parameter of type {@code Locale} used by this operation.
     * @param key Parameter of type {@code String} used by this operation.
     * @param args Parameter of type {@code Object...} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String msg(Locale locale, String key, Object... args) {
        return i18nService.msg(locale, key, args);
    }

    public record ReceiptPrintPayload(
            String jobType,
            Long saleId,
            String terminalId,
            String text,
            String qrPayload,
            List<Map<String, Object>> lines,
            Map<String, Object> totals,
            List<Map<String, Object>> payments,
            Map<String, Object> metadata,
            boolean cut,
            String encoding
    ) {
    }
}
