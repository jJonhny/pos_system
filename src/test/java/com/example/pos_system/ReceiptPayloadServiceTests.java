package com.example.pos_system;

import com.example.pos_system.entity.PaymentMethod;
import com.example.pos_system.entity.PrinterMode;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SaleItem;
import com.example.pos_system.entity.SalePayment;
import com.example.pos_system.entity.TerminalSettings;
import com.example.pos_system.service.ReceiptPayloadService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@ActiveProfiles("test")
class ReceiptPayloadServiceTests {

    @Autowired
    private ReceiptPayloadService receiptPayloadService;

    /**
     * Executes the buildPrintPayloadIncludesTotalsTaxesAndSplitPayments operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the buildPrintPayloadIncludesTotalsTaxesAndSplitPayments operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the buildPrintPayloadIncludesTotalsTaxesAndSplitPayments operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void buildPrintPayloadIncludesTotalsTaxesAndSplitPayments() {
        Sale sale = buildSale();
        TerminalSettings settings = buildSettings();

        ReceiptPayloadService.ReceiptPrintPayload payload = receiptPayloadService.buildPrintPayload(sale, settings, "TERM-01");

        assertThat(payload.saleId()).isEqualTo(101L);
        assertThat(payload.terminalId()).isEqualTo("TERM-01");
        assertThat(payload.totals()).containsEntry("subtotal", new BigDecimal("100.00"));
        assertThat(payload.totals()).containsEntry("discount", new BigDecimal("10.00"));
        assertThat(payload.totals()).containsEntry("tax", new BigDecimal("4.50"));
        assertThat(payload.totals()).containsEntry("total", new BigDecimal("94.50"));
        assertThat(payload.payments()).hasSize(2);
        assertThat(payload.payments().getFirst()).containsEntry("displayCashChange", false);
        assertThat(payload.payments().getFirst()).containsKey("cashReceivedBase");
        assertThat(payload.text()).contains("Subtotal");
        assertThat(payload.text()).contains("Discount");
        assertThat(payload.text()).contains("Tax");
        assertThat(payload.text()).contains("TOTAL");
        assertThat(payload.text()).contains("CASH");
        assertThat(payload.text()).contains("CARD");
        assertThat(payload.text()).contains("Cash Received");
        assertThat(payload.text()).doesNotContain("  Change");
        assertThat(payload.text()).contains("200000.00 KHR");
    }

    /**
     * Executes the buildPrintPayloadIncludesCashReceivedAndChangeForSingleCashPayment operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the buildPrintPayloadIncludesCashReceivedAndChangeForSingleCashPayment operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the buildPrintPayloadIncludesCashReceivedAndChangeForSingleCashPayment operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void buildPrintPayloadIncludesCashReceivedAndChangeForSingleCashPayment() {
        Sale sale = buildSale();
        sale.getPayments().clear();

        SalePayment cash = new SalePayment();
        cash.setMethod(PaymentMethod.CASH);
        cash.setAmount(new BigDecimal("94.50"));
        cash.setCurrencyCode("USD");
        cash.setCurrencyRate(new BigDecimal("1.0000"));
        cash.setForeignAmount(new BigDecimal("100.00"));
        sale.getPayments().add(cash);

        ReceiptPayloadService.ReceiptPrintPayload payload = receiptPayloadService.buildPrintPayload(sale, buildSettings(), "TERM-01");

        assertThat(payload.text()).contains("Cash Received");
        assertThat(payload.text()).contains("Change");
        assertThat(payload.text()).contains("$100.00");
        assertThat(payload.text()).contains("$5.50");
    }

    /**
     * Executes the buildPrintPayloadIncludesHeaderFooterAndQrPayload operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the buildPrintPayloadIncludesHeaderFooterAndQrPayload operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the buildPrintPayloadIncludesHeaderFooterAndQrPayload operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void buildPrintPayloadIncludesHeaderFooterAndQrPayload() {
        Sale sale = buildSale();
        TerminalSettings settings = buildSettings();

        ReceiptPayloadService.ReceiptPrintPayload payload = receiptPayloadService.buildPrintPayload(sale, settings, "TERM-99");

        assertThat(payload.text()).contains("My Store Header");
        assertThat(payload.text()).contains("Thank you and come again");
        assertThat(payload.text()).contains("Tax ID");
        assertThat(payload.qrPayload()).contains("sale=101");
        assertThat(payload.qrPayload()).contains("terminal=TERM-99");
    }

    /**
     * Executes the buildSale operation.
     *
     * @return {@code Sale} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Sale buildSale() {
        Sale sale = new Sale();
        sale.setId(101L);
        sale.setCreatedAt(LocalDateTime.of(2026, 2, 14, 10, 30));
        sale.setCashierUsername("cashier");
        sale.setSubtotal(new BigDecimal("100.00"));
        sale.setDiscount(new BigDecimal("10.00"));
        sale.setTax(new BigDecimal("4.50"));
        sale.setTotal(new BigDecimal("94.50"));

        Product product = new Product();
        product.setId(55L);
        product.setName("Arabica Coffee");

        SaleItem item = new SaleItem();
        item.setProduct(product);
        item.setQty(2);
        item.setUnitPrice(new BigDecimal("50.00"));
        item.setLineTotal(new BigDecimal("100.00"));
        sale.getItems().add(item);

        SalePayment cash = new SalePayment();
        cash.setMethod(PaymentMethod.CASH);
        cash.setAmount(new BigDecimal("50.00"));
        cash.setCurrencyCode("KHR");
        cash.setCurrencyRate(new BigDecimal("0.00025"));
        cash.setForeignAmount(new BigDecimal("200000.00"));

        SalePayment card = new SalePayment();
        card.setMethod(PaymentMethod.CARD);
        card.setAmount(new BigDecimal("44.50"));

        sale.getPayments().add(cash);
        sale.getPayments().add(card);
        return sale;
    }

    /**
     * Executes the buildSettings operation.
     *
     * @return {@code TerminalSettings} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private TerminalSettings buildSettings() {
        TerminalSettings settings = new TerminalSettings();
        settings.setTerminalId("TERM-01");
        settings.setName("Front Counter");
        settings.setPrinterMode(PrinterMode.BRIDGE);
        settings.setBridgeUrl("http://127.0.0.1:18765");
        settings.setReceiptHeader("My Store Header");
        settings.setReceiptFooter("Thank you and come again");
        settings.setTaxId("VAT-123");
        return settings;
    }
}
