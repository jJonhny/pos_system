package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.repository.ProductRepo;
import com.devcore.pos_system.service.ProductUnitAdminService.ProductUnitDraft;
import com.devcore.pos_system.service.ProductUnitAdminService.ProductUnitValidationException;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SpringBootTest
@ActiveProfiles("test")
class ProductUnitAdminServiceValidationTest {

    @Autowired
    private ProductUnitAdminService productUnitAdminService;
    @Autowired
    private ProductRepo productRepo;

    @Test
    void shouldRejectAbbreviationEqualToBaseUnitName() {
        Product product = createProduct("piece");

        ProductUnitValidationException ex = assertThrows(
                ProductUnitValidationException.class,
                () -> productUnitAdminService.replaceUnits(product, List.of(
                        new ProductUnitDraft(null, "Bottle", "piece", new BigDecimal("12"), true, true, null)
                ), 0, 0)
        );

        assertTrue(ex.getMessage().toLowerCase().contains("abbreviation"));
    }

    @Test
    void shouldRejectConversionOutOfRange() {
        Product product = createProduct("bottle");

        assertThrows(
                ProductUnitValidationException.class,
                () -> productUnitAdminService.replaceUnits(product, List.of(
                        new ProductUnitDraft(null, "Case", "CS", new BigDecimal("0.00001"), true, true, null)
                ), 0, 0)
        );
    }

    @Test
    void shouldRejectDefaultSaleUnitWhenSellFlagIsFalse() {
        Product product = createProduct("egg");

        assertThrows(
                ProductUnitValidationException.class,
                () -> productUnitAdminService.replaceUnits(product, List.of(
                        new ProductUnitDraft(null, "Tray", "TRY", new BigDecimal("30"), false, true, null)
                ), 0, 0)
        );
    }

    @Test
    void shouldRejectDuplicateBarcodeInsideSameProductRows() {
        Product product = createProduct("can");

        assertThrows(
                ProductUnitValidationException.class,
                () -> productUnitAdminService.replaceUnits(product, List.of(
                        new ProductUnitDraft(null, "Pack", "PK", new BigDecimal("6"), true, true, "ABC-1"),
                        new ProductUnitDraft(null, "Case", "CS", new BigDecimal("24"), true, true, "abc-1")
                ), 0, 1)
        );
    }

    private Product createProduct(String baseUnitName) {
        Product product = new Product();
        product.setName("Validation Product " + UUID.randomUUID());
        product.setSku("uom-validate-" + UUID.randomUUID());
        product.setPrice(new BigDecimal("1.00"));
        product.setStockQty(0);
        product.setBaseUnitName(baseUnitName);
        product.setBaseUnitPrecision(0);
        return productRepo.save(product);
    }
}

