package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.ProductUnit;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class ProductUnitLegacyFallbackTest {

    @Test
    void applyLegacyFallbackUsesMinAndMaxConversions() {
        Product product = new Product();
        ProductUnitAdminService service = new ProductUnitAdminService(null, false);

        service.applyLegacyFallbackFields(product, List.of(
                unit("Bundle", "BND", "12"),
                unit("Carton", "CTN", "30"),
                unit("Pack", "PK", "24")
        ));

        assertEquals(12, product.getUnitsPerBox());
        assertEquals(30, product.getUnitsPerCase());
    }

    @Test
    void applyLegacyFallbackKeepsSingleUnitAsOnlyLegacySlot() {
        Product product = new Product();
        ProductUnitAdminService service = new ProductUnitAdminService(null, false);

        service.applyLegacyFallbackFields(product, List.of(unit("Tray", "TRY", "10")));

        assertEquals(10, product.getUnitsPerBox());
        assertNull(product.getUnitsPerCase());
    }

    @Test
    void feedItemInfersLegacySizesFromDynamicUnitsWithoutBoxCaseNames() {
        Product product = new Product();
        product.setId(100L);
        product.setName("Egg");
        product.setSku("EGG-001");
        product.setProductUnits(List.of(
                unit("Bundle", "BND", "12"),
                unit("Carton", "CTN", "30")
        ));

        ProductFeedService feedService = new ProductFeedService(null, null);
        ProductFeedService.ProductFeedItem item = feedService.toFeedItem(product);

        assertEquals(12, item.unitsPerBox());
        assertEquals(30, item.unitsPerCase());
    }

    private ProductUnit unit(String name, String abbreviation, String conversionToBase) {
        ProductUnit unit = new ProductUnit();
        unit.setName(name);
        unit.setAbbreviation(abbreviation);
        unit.setConversionToBase(new BigDecimal(conversionToBase));
        unit.setAllowForSale(true);
        unit.setAllowForPurchase(true);
        return unit;
    }
}
