package com.example.pos_system;

import com.example.pos_system.entity.PaymentMethod;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SalePayment;
import com.example.pos_system.entity.SaleStatus;
import com.example.pos_system.service.ReceiptPdfService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@ActiveProfiles("test")
class ReceiptLocaleIntegrationTest {

    @Autowired
    private ReceiptPdfService receiptPdfService;

    /**
     * Executes the receiptPdfUsesSaleLocaleWhenProvided operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the receiptPdfUsesSaleLocaleWhenProvided operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the receiptPdfUsesSaleLocaleWhenProvided operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void receiptPdfUsesSaleLocaleWhenProvided() {
        Sale sale = new Sale();
        sale.setId(2002L);
        sale.setCreatedAt(LocalDateTime.now());
        sale.setStatus(SaleStatus.PAID);
        sale.setPaymentMethod(PaymentMethod.CASH);
        sale.setReceiptLocale("zh-CN");
        sale.setSubtotal(new BigDecimal("10.00"));
        sale.setDiscount(BigDecimal.ZERO);
        sale.setTax(BigDecimal.ZERO);
        sale.setTotal(new BigDecimal("10.00"));
        sale.setRefundedTotal(BigDecimal.ZERO);

        SalePayment cash = new SalePayment();
        cash.setMethod(PaymentMethod.CASH);
        cash.setAmount(new BigDecimal("10.00"));
        cash.setCurrencyCode("USD");
        cash.setCurrencyRate(new BigDecimal("1.0000"));
        cash.setForeignAmount(new BigDecimal("12.00"));
        sale.getPayments().add(cash);

        String html = receiptPdfService.renderReceiptPdf(sale);

        assertThat(html).contains("收据 PDF");
        assertThat(html).contains("实收现金");
        assertThat(html).contains("找零");
    }
}
