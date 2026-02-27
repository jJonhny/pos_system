package com.example.pos_system;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.dto.CartItem;
import com.example.pos_system.dto.VariantApiDtos;
import com.example.pos_system.entity.Category;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.ProductVariant;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SaleItem;
import com.example.pos_system.entity.SkuSellUnit;
import com.example.pos_system.modules.currency.application.CurrencyService;
import com.example.pos_system.modules.currency.domain.Currency;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.ProductVariantRepo;
import com.example.pos_system.repository.SaleRepo;
import com.example.pos_system.repository.UnitOfMeasureRepo;
import com.example.pos_system.service.ShiftService;
import com.example.pos_system.service.SkuUnitPricingService;
import com.example.pos_system.service.VariantCombinationKeyService;
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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
class PosVariantScanAndCheckoutIntegrationTest {
    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private CategoryRepo categoryRepo;
    @Autowired
    private ProductRepo productRepo;
    @Autowired
    private ProductVariantRepo productVariantRepo;
    @Autowired
    private UnitOfMeasureRepo unitOfMeasureRepo;
    @Autowired
    private SkuUnitPricingService skuUnitPricingService;
    @Autowired
    private VariantCombinationKeyService keyService;
    @Autowired
    private ShiftService shiftService;
    @Autowired
    private CurrencyService currencyService;
    @Autowired
    private SaleRepo saleRepo;

    /**
     * Executes the scanAndCheckoutUseVariantBarcodeFlow operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the scanAndCheckoutUseVariantBarcodeFlow operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the scanAndCheckoutUseVariantBarcodeFlow operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void scanAndCheckoutUseVariantBarcodeFlow() throws Exception {
        VariantFixture fixture = createVariantFixture();
        Currency baseCurrency = currencyService.getBaseCurrency();
        String baseCode = baseCurrency.getCode();
        shiftService.openShift("cashier", "TERM-VARIANT-01", Map.of(baseCode, new BigDecimal("50.00")));

        MockHttpSession session = (MockHttpSession) mockMvc.perform(post("/pos/scan")
                        .header("HX-Request", "true")
                        .param("barcode", fixture.scanBarcode())
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().isOk())
                .andReturn()
                .getRequest()
                .getSession(false);

        Object cartObject = session.getAttribute("cart");
        assertThat(cartObject).isInstanceOf(Cart.class);
        Cart cart = (Cart) cartObject;
        assertThat(cart.getItems()).hasSize(1);
        CartItem line = cart.getItems().iterator().next();
        assertThat(line.getVariantId()).isEqualTo(fixture.variantId());
        assertThat(line.getSellUnitId()).isEqualTo(fixture.sellUnitId());
        assertThat(line.getSellUnitCode()).isEqualTo("PIECE");
        assertThat(line.getUnitPrice()).isEqualByComparingTo("1.25");

        long salesBefore = saleRepo.count();
        mockMvc.perform(post("/pos/checkout")
                        .session(session)
                        .header("HX-Request", "true")
                        .param("method", "CASH")
                        .param("terminalId", "TERM-VARIANT-01")
                        .param("clientCheckoutId", UUID.randomUUID().toString())
                        .param("currencyCode", baseCode)
                        .param("currencyRate", "1.0000")
                        .param("foreignAmount", "2.00")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().isOk());

        assertThat(saleRepo.count()).isEqualTo(salesBefore + 1);
        Sale latest = saleRepo.findAll().stream()
                .max(java.util.Comparator.comparing(Sale::getId))
                .orElseThrow();
        assertThat(latest.getItems()).hasSize(1);
        SaleItem sold = latest.getItems().getFirst();
        assertThat(sold.getVariantId()).isEqualTo(fixture.variantId());
        assertThat(sold.getSellUnitId()).isEqualTo(fixture.sellUnitId());
        assertThat(sold.getSellUnitCode()).isEqualTo("PIECE");
        assertThat(sold.getConversionToBase()).isEqualByComparingTo("1.000000");

        ProductVariant refreshed = productVariantRepo.findById(fixture.variantId()).orElseThrow();
        assertThat(refreshed.getStockBaseQty()).isEqualByComparingTo("11.000000");
    }

    /**
     * Executes the createVariantFixture operation.
     *
     * @return {@code VariantFixture} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private VariantFixture createVariantFixture() {
        Category category = new Category();
        category.setName("Variant POS Category");
        category.setActive(true);
        category.setSortOrder(1);
        Category savedCategory = categoryRepo.save(category);

        Product product = new Product();
        product.setName("Spark Drink");
        product.setSku("VAR-DRINK-001");
        product.setPrice(new BigDecimal("2.00"));
        product.setStockQty(50);
        product.setActive(true);
        product.setAllowNegativeStock(false);
        product.setCategory(savedCategory);
        Product savedProduct = productRepo.save(product);

        ProductVariant variant = new ProductVariant();
        variant.setProduct(savedProduct);
        variant.setCombinationKey("SIZE=330ML|FLAVOR=COLA");
        variant.setCombinationHash(keyService.hash("SIZE=330ML|FLAVOR=COLA"));
        variant.setVariantName("330ML / Cola");
        variant.setEnabled(true);
        variant.setImpossible(false);
        variant.setArchived(false);
        variant.setPrice(new BigDecimal("1.25"));
        variant.setStockBaseQty(new BigDecimal("12.000000"));
        ProductVariant savedVariant = productVariantRepo.save(variant);

        if (unitOfMeasureRepo.findByCodeIgnoreCase("PIECE").isEmpty()) {
            skuUnitPricingService.createUnit(new VariantApiDtos.UnitCreateRequest("PIECE", "Piece", 0));
        }
        SkuSellUnit sellUnit = skuUnitPricingService.upsertSellUnit(savedVariant.getId(),
                new VariantApiDtos.SellUnitUpsertRequest("PIECE", BigDecimal.ONE, true, new BigDecimal("1.25"), true));

        String barcode = "VAR-SCAN-" + UUID.randomUUID().toString().substring(0, 8).toUpperCase();
        skuUnitPricingService.addBarcode(sellUnit.getId(), new VariantApiDtos.BarcodeCreateRequest(barcode, true));
        return new VariantFixture(savedVariant.getId(), sellUnit.getId(), barcode);
    }

    private record VariantFixture(Long variantId, Long sellUnitId, String scanBarcode) {}
}
