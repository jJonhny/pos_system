package com.example.pos_system;

import com.example.pos_system.entity.Category;
import com.example.pos_system.entity.Currency;
import com.example.pos_system.entity.Product;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.SaleRepo;
import com.example.pos_system.service.CurrencyService;
import com.example.pos_system.service.ShiftService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
class PosCashTenderIntegrationTest {
    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private CategoryRepo categoryRepo;
    @Autowired
    private ProductRepo productRepo;
    @Autowired
    private SaleRepo saleRepo;
    @Autowired
    private ShiftService shiftService;
    @Autowired
    private CurrencyService currencyService;

    @Test
    void checkoutRejectsCashWhenReceivedAmountIsBelowTotal() throws Exception {
        Currency baseCurrency = currencyService.getBaseCurrency();
        String currencyCode = baseCurrency.getCode();
        shiftService.openShift("cashier", "TERM-CASH-01", Map.of(currencyCode, new BigDecimal("100.00")));

        Product product = createProduct(new BigDecimal("10.00"));
        long salesBefore = saleRepo.count();

        MockHttpSession session = (MockHttpSession) mockMvc.perform(post("/pos/cart/add/{id}", product.getId())
                        .header("HX-Request", "true")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().isOk())
                .andReturn()
                .getRequest()
                .getSession(false);

        mockMvc.perform(post("/pos/checkout")
                        .session(session)
                        .header("HX-Request", "true")
                        .param("method", "CASH")
                        .param("terminalId", "TERM-CASH-01")
                        .param("clientCheckoutId", UUID.randomUUID().toString())
                        .param("currencyCode", currencyCode)
                        .param("currencyRate", "1.0000")
                        .param("foreignAmount", "5.00")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("Entered cash is less than the total bill.")));

        assertThat(saleRepo.count()).isEqualTo(salesBefore);
    }

    private Product createProduct(BigDecimal price) {
        Category category = new Category();
        category.setName("POS Cash Tender Test Category");
        category.setActive(true);
        category.setSortOrder(1);
        Category savedCategory = categoryRepo.save(category);

        Product product = new Product();
        product.setName("POS Cash Tender Test Product");
        product.setSku("POS-CASH-001");
        product.setPrice(price);
        product.setStockQty(30);
        product.setActive(true);
        product.setCategory(savedCategory);
        return productRepo.save(product);
    }
}
