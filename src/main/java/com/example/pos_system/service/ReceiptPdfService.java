package com.example.pos_system.service;

import com.example.pos_system.entity.Sale;
import org.springframework.stereotype.Service;
import org.thymeleaf.context.Context;
import org.thymeleaf.spring6.SpringTemplateEngine;

import java.time.LocalDateTime;

@Service
public class ReceiptPdfService {
    private final SpringTemplateEngine templateEngine;

    public ReceiptPdfService(SpringTemplateEngine templateEngine) {
        this.templateEngine = templateEngine;
    }

    public String renderReceiptPdf(Sale sale) {
        Context context = new Context();
        context.setVariable("sale", sale);
        context.setVariable("generatedAt", LocalDateTime.now());
        return templateEngine.process("sales/receipt_pdf", context);
    }
}
