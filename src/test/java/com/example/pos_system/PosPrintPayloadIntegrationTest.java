package com.example.pos_system;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.entity.Category;
import com.example.pos_system.entity.PaymentMethod;
import com.example.pos_system.entity.PrinterMode;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SalePayment;
import com.example.pos_system.entity.Shift;
import com.example.pos_system.entity.ShiftStatus;
import com.example.pos_system.entity.TerminalSettings;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.ShiftRepo;
import com.example.pos_system.repository.TerminalSettingsRepo;
import com.example.pos_system.service.PosService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
class PosPrintPayloadIntegrationTest {

    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private CategoryRepo categoryRepo;
    @Autowired
    private ProductRepo productRepo;
    @Autowired
    private ShiftRepo shiftRepo;
    @Autowired
    private TerminalSettingsRepo terminalSettingsRepo;
    @Autowired
    private PosService posService;

    /**
     * Executes the checkoutPrintPayloadEndpointReturnsBridgeStructure operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the checkoutPrintPayloadEndpointReturnsBridgeStructure operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the checkoutPrintPayloadEndpointReturnsBridgeStructure operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void checkoutPrintPayloadEndpointReturnsBridgeStructure() throws Exception {
        String terminalId = "TERM-PRINT-01";
        Product product = createProduct();
        Shift shift = createShift(terminalId);
        createTerminalSettings(terminalId);

        Cart cart = new Cart();
        cart.add(product);

        SalePayment payment = new SalePayment();
        payment.setMethod(PaymentMethod.CASH);
        payment.setAmount(cart.getTotal());

        Sale sale = posService.checkout(cart, payment, "cashier", null, shift, terminalId);

        mockMvc.perform(post("/pos/checkout/{saleId}/print", sale.getId())
                        .param("terminalId", terminalId)
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().isOk())
                .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.saleId").value(sale.getId()))
                .andExpect(jsonPath("$.terminalId").value(terminalId))
                .andExpect(jsonPath("$.printerMode").value("BRIDGE"))
                .andExpect(jsonPath("$.payload.jobType").value("receipt"))
                .andExpect(jsonPath("$.payload.text", containsString("TOTAL")));
    }

    /**
     * Executes the createProduct operation.
     *
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Product createProduct() {
        Category category = new Category();
        category.setName("Print Category");
        category.setActive(true);
        category.setSortOrder(1);
        Category savedCategory = categoryRepo.save(category);

        Product product = new Product();
        product.setName("Print Product");
        product.setSku("PRINT-001");
        product.setPrice(new BigDecimal("12.50"));
        product.setStockQty(50);
        product.setActive(true);
        product.setCategory(savedCategory);
        return productRepo.save(product);
    }

    /**
     * Executes the createShift operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code Shift} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Shift createShift(String terminalId) {
        Shift shift = new Shift();
        shift.setCashierUsername("cashier");
        shift.setTerminalId(terminalId);
        shift.setOpenedAt(LocalDateTime.now());
        shift.setStatus(ShiftStatus.OPEN);
        shift.setOpeningCash(new BigDecimal("100.00"));
        return shiftRepo.save(shift);
    }

    /**
     * Executes the createTerminalSettings operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void createTerminalSettings(String terminalId) {
        TerminalSettings settings = new TerminalSettings();
        settings.setTerminalId(terminalId);
        settings.setName("Front Counter");
        settings.setDefaultCurrency("USD");
        settings.setReceiptHeader("POS Test Header");
        settings.setReceiptFooter("POS Test Footer");
        settings.setPrinterMode(PrinterMode.BRIDGE);
        settings.setBridgeUrl("http://127.0.0.1:18765");
        settings.setAutoPrintEnabled(true);
        settings.setCameraScannerEnabled(false);
        terminalSettingsRepo.save(settings);
    }
}
