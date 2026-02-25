package com.example.pos_system;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.entity.Category;
import com.example.pos_system.entity.Currency;
import com.example.pos_system.entity.MarketingCampaign;
import com.example.pos_system.entity.MarketingCampaignType;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.MarketingCampaignRepo;
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
import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.Map;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
class PosMarketingCampaignIntegrationTest {
    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private CategoryRepo categoryRepo;
    @Autowired
    private ProductRepo productRepo;
    @Autowired
    private MarketingCampaignRepo marketingCampaignRepo;
    @Autowired
    private SaleRepo saleRepo;
    @Autowired
    private ShiftService shiftService;
    @Autowired
    private CurrencyService currencyService;

    @Test
    void activeCampaignAutoAppliesToCartAndCheckoutTotals() throws Exception {
        createActiveDiscountCampaign("AUTO-10", new BigDecimal("10.00"), null);
        Product product = createProduct("POS-MKT-001", new BigDecimal("20.00"));

        Currency baseCurrency = currencyService.getBaseCurrency();
        String currencyCode = baseCurrency.getCode();
        shiftService.openShift("cashier", "TERM-MKT-01", Map.of(currencyCode, new BigDecimal("100.00")));

        MockHttpSession session = (MockHttpSession) mockMvc.perform(post("/pos/cart/add/{id}", product.getId())
                        .header("HX-Request", "true")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("Auto campaign:")))
                .andReturn()
                .getRequest()
                .getSession(false);

        Cart cart = (Cart) session.getAttribute("cart");
        assertThat(cart).isNotNull();
        assertThat(cart.getDiscount()).isEqualByComparingTo("2.00");
        assertThat(cart.getDiscountReason()).startsWith("[AUTO_CAMPAIGN] ");
        assertThat(cart.isManualDiscountOverride()).isFalse();

        long salesBefore = saleRepo.count();
        mockMvc.perform(post("/pos/checkout")
                        .session(session)
                        .header("HX-Request", "true")
                        .param("method", "CASH")
                        .param("terminalId", "TERM-MKT-01")
                        .param("clientCheckoutId", UUID.randomUUID().toString())
                        .param("currencyCode", currencyCode)
                        .param("currencyRate", "1.0000")
                        .param("foreignAmount", "25.00")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().isOk());

        assertThat(saleRepo.count()).isEqualTo(salesBefore + 1);
        Sale latest = saleRepo.findAll().stream()
                .max(Comparator.comparing(Sale::getId))
                .orElseThrow();
        assertThat(latest.getDiscount()).isEqualByComparingTo("2.00");
        assertThat(latest.getTotal()).isEqualByComparingTo("18.00");
        assertThat(latest.getDiscountReason()).startsWith("[AUTO_CAMPAIGN] ");
    }

    @Test
    void manualDiscountOverridesAutoCampaignUntilCleared() throws Exception {
        createActiveDiscountCampaign("AUTO-50", new BigDecimal("50.00"), null);
        Product product = createProduct("POS-MKT-002", new BigDecimal("30.00"));

        MockHttpSession session = (MockHttpSession) mockMvc.perform(post("/pos/cart/add/{id}", product.getId())
                        .header("HX-Request", "true")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("Auto campaign:")))
                .andReturn()
                .getRequest()
                .getSession(false);

        Cart cartWithAuto = (Cart) session.getAttribute("cart");
        assertThat(cartWithAuto.getDiscount()).isEqualByComparingTo("15.00");
        assertThat(cartWithAuto.isManualDiscountOverride()).isFalse();

        mockMvc.perform(post("/pos/cart/discount")
                        .session(session)
                        .header("HX-Request", "true")
                        .param("discountType", "AMOUNT")
                        .param("discountValue", "1.00")
                        .param("discountReason", "Manual manager approval")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().isOk())
                .andExpect(content().string(not(containsString("Auto campaign:"))));

        Cart cartAfterManual = (Cart) session.getAttribute("cart");
        assertThat(cartAfterManual.getDiscount()).isEqualByComparingTo("1.00");
        assertThat(cartAfterManual.getDiscountReason()).isEqualTo("Manual manager approval");
        assertThat(cartAfterManual.isManualDiscountOverride()).isTrue();

        mockMvc.perform(post("/pos/cart/update")
                        .session(session)
                        .header("HX-Request", "true")
                        .param("productId", String.valueOf(product.getId()))
                        .param("qty", "2")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().isOk())
                .andExpect(content().string(not(containsString("Auto campaign:"))));

        Cart cartAfterQtyUpdate = (Cart) session.getAttribute("cart");
        assertThat(cartAfterQtyUpdate.getDiscount()).isEqualByComparingTo("1.00");
        assertThat(cartAfterQtyUpdate.isManualDiscountOverride()).isTrue();

        mockMvc.perform(post("/pos/cart/discount")
                        .session(session)
                        .header("HX-Request", "true")
                        .param("discountType", "AMOUNT")
                        .param("discountValue", "0")
                        .param("discountReason", "")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("Auto campaign:")));

        Cart cartAfterClear = (Cart) session.getAttribute("cart");
        assertThat(cartAfterClear.isManualDiscountOverride()).isFalse();
        assertThat(cartAfterClear.getDiscount()).isEqualByComparingTo("30.00");
        assertThat(cartAfterClear.getDiscountReason()).startsWith("[AUTO_CAMPAIGN] ");
    }

    private void createActiveDiscountCampaign(String title, BigDecimal discountPercent, BigDecimal discountAmount) {
        MarketingCampaign campaign = new MarketingCampaign();
        campaign.setType(MarketingCampaignType.DISCOUNT_EVENT);
        campaign.setTitle(title);
        campaign.setDescription("POS marketing campaign test");
        campaign.setDiscountPercent(discountPercent);
        campaign.setDiscountAmount(discountAmount);
        campaign.setMinSpend(BigDecimal.ZERO);
        campaign.setStartsAt(LocalDateTime.now().minusHours(1));
        campaign.setEndsAt(LocalDateTime.now().plusHours(2));
        campaign.setActive(true);
        marketingCampaignRepo.save(campaign);
    }

    private Product createProduct(String sku, BigDecimal price) {
        Category category = new Category();
        category.setName("POS Marketing Test Category " + sku);
        category.setActive(true);
        category.setSortOrder(1);
        Category savedCategory = categoryRepo.save(category);

        Product product = new Product();
        product.setName("POS Marketing Test Product " + sku);
        product.setSku(sku);
        product.setPrice(price);
        product.setStockQty(30);
        product.setActive(true);
        product.setCategory(savedCategory);
        return productRepo.save(product);
    }
}
