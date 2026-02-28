package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.ProductUnit;
import com.devcore.pos_system.repository.ProductRepo;
import com.devcore.pos_system.repository.ProductUnitRepo;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

@SpringBootTest
@ActiveProfiles("test")
class ProductUnitConversionServiceTest {
    @Autowired
    private ProductUnitConversionService productUnitConversionService;
    @Autowired
    private ProductRepo productRepo;
    @Autowired
    private ProductUnitRepo productUnitRepo;

    @Test
    void toBaseAndFromBaseShouldUsePersistedProductUnits() {
        Product product = new Product();
        product.setName("UOM Product");
        product.setSku("uom-test-1");
        product.setPrice(new BigDecimal("1.00"));
        product.setStockQty(0);
        product.setBaseUnitPrecision(0);
        Product savedProduct = productRepo.save(product);

        ProductUnit unit = new ProductUnit();
        unit.setProduct(savedProduct);
        unit.setName("Case");
        unit.setAbbreviation("CS");
        unit.setConversionToBase(new BigDecimal("24"));
        unit.setAllowForSale(true);
        unit.setAllowForPurchase(true);
        ProductUnit savedUnit = productUnitRepo.save(unit);

        BigDecimal toBase = productUnitConversionService.toBase(savedProduct.getId(), savedUnit.getId(), new BigDecimal("2"));
        BigDecimal fromBase = productUnitConversionService.fromBase(savedProduct.getId(), savedUnit.getId(), new BigDecimal("48"));

        assertEquals(new BigDecimal("48"), toBase);
        assertEquals(new BigDecimal("2"), fromBase);
    }

    @Test
    void toBaseShouldRejectUnitFromDifferentProduct() {
        Product first = new Product();
        first.setName("Owner A");
        first.setSku("uom-test-2");
        first.setPrice(new BigDecimal("1.00"));
        first.setStockQty(0);
        first.setBaseUnitPrecision(0);
        Product ownerA = productRepo.save(first);

        Product second = new Product();
        second.setName("Owner B");
        second.setSku("uom-test-3");
        second.setPrice(new BigDecimal("1.00"));
        second.setStockQty(0);
        second.setBaseUnitPrecision(0);
        Product ownerB = productRepo.save(second);

        ProductUnit unit = new ProductUnit();
        unit.setProduct(ownerB);
        unit.setName("Bundle");
        unit.setConversionToBase(new BigDecimal("12"));
        unit.setAllowForSale(true);
        unit.setAllowForPurchase(true);
        ProductUnit savedUnit = productUnitRepo.save(unit);

        IllegalArgumentException ex = assertThrows(
                IllegalArgumentException.class,
                () -> productUnitConversionService.toBase(ownerA.getId(), savedUnit.getId(), BigDecimal.ONE)
        );
        assertEquals("Unit does not belong to this product.", ex.getMessage());
    }
}
