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

    public ReceiptPdfService(SpringTemplateEngine templateEngine,
                             UiFormat uiFormat,
                             ReceiptPaymentService receiptPaymentService,
                             I18nService i18nService) {
        this.templateEngine = templateEngine;
        this.uiFormat = uiFormat;
        this.receiptPaymentService = receiptPaymentService;
        this.i18nService = i18nService;
    }

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

    private Locale resolveReceiptLocale(Sale sale) {
        if (sale != null && sale.getReceiptLocale() != null && !sale.getReceiptLocale().isBlank()) {
            return i18nService.parseOrDefault(sale.getReceiptLocale());
        }
        return i18nService.parseOrDefault(LocaleContextHolder.getLocale().toLanguageTag());
    }
}
