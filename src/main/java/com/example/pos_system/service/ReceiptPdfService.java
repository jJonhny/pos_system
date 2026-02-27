package com.example.pos_system.service;

import com.example.pos_system.entity.Sale;
import com.example.pos_system.util.UiFormat;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;
import org.thymeleaf.context.Context;
import org.thymeleaf.spring6.SpringTemplateEngine;

import java.time.LocalDateTime;
import java.util.Locale;

@Service
public class ReceiptPdfService {
    private final SpringTemplateEngine templateEngine;
    private final UiFormat uiFormat;
    private final ReceiptPaymentService receiptPaymentService;
    private final I18nService i18nService;

    /**
     * Executes the ReceiptPdfService operation.
     * <p>Return value: A fully initialized ReceiptPdfService instance.</p>
     *
     * @param templateEngine Parameter of type {@code SpringTemplateEngine} used by this operation.
     * @param uiFormat Parameter of type {@code UiFormat} used by this operation.
     * @param receiptPaymentService Parameter of type {@code ReceiptPaymentService} used by this operation.
     * @param i18nService Parameter of type {@code I18nService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ReceiptPdfService(SpringTemplateEngine templateEngine,
                             UiFormat uiFormat,
                             ReceiptPaymentService receiptPaymentService,
                             I18nService i18nService) {
        this.templateEngine = templateEngine;
        this.uiFormat = uiFormat;
        this.receiptPaymentService = receiptPaymentService;
        this.i18nService = i18nService;
    }

    /**
     * Executes the renderReceiptPdf operation.
     *
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String renderReceiptPdf(Sale sale) {
        Locale receiptLocale = resolveReceiptLocale(sale);
        Context context = new Context(receiptLocale);
        var receiptPaymentLines = receiptPaymentService.buildLines(sale);
        context.setVariable("sale", sale);
        context.setVariable("generatedAt", LocalDateTime.now());
        context.setVariable("uiFormat", uiFormat);
        context.setVariable("receiptPaymentLines", receiptPaymentLines);
        context.setVariable("cashReceivedBase", receiptPaymentService.totalCashReceivedBase(receiptPaymentLines));
        context.setVariable("cashChangeBase", receiptPaymentService.totalCashChangeBase(receiptPaymentLines));
        context.setVariable("receiptLocale", receiptLocale);
        return templateEngine.process("sales/receipt_pdf", context);
    }

    /**
     * Executes the resolveReceiptLocale operation.
     *
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @return {@code Locale} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Locale resolveReceiptLocale(Sale sale) {
        if (sale != null && sale.getReceiptLocale() != null && !sale.getReceiptLocale().isBlank()) {
            return i18nService.parseOrDefault(sale.getReceiptLocale());
        }
        return i18nService.parseOrDefault(LocaleContextHolder.getLocale().toLanguageTag());
    }
}
