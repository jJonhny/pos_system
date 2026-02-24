package com.example.pos_system;

import com.example.pos_system.dto.VariantApiDtos;
import com.example.pos_system.entity.AttributeGroup;
import com.example.pos_system.entity.AttributeValue;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.ProductVariant;
import com.example.pos_system.entity.SkuSellUnit;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.ProductVariantRepo;
import com.example.pos_system.service.ProductVariantService;
import com.example.pos_system.service.InventoryService;
import com.example.pos_system.service.PricingService;
import com.example.pos_system.service.SkuUnitPricingService;
import com.example.pos_system.service.VariantCombinationKeyService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@ActiveProfiles("test")
@Transactional
class VariantAndPricingServiceIntegrationTest {

    @Autowired
    private ProductRepo productRepo;

    @Autowired
    private ProductVariantRepo productVariantRepo;

    @Autowired
    private ProductVariantService productVariantService;

    @Autowired
    private SkuUnitPricingService skuUnitPricingService;

    @Autowired
    private PricingService pricingService;

    @Autowired
    private InventoryService inventoryService;

    @Autowired
    private VariantCombinationKeyService keyService;

    @Test
    void variantGenerationPreservesEditedRowsWhenAttributesChange() {
        Product product = new Product();
        product.setName("Spark Drink");
        product.setPrice(new BigDecimal("1.50"));
        product.setCostPrice(new BigDecimal("0.80"));
        product = productRepo.save(product);

        AttributeGroup size = productVariantService.createAttributeGroup(new VariantApiDtos.AttributeGroupCreateRequest("SIZE", "Size", 10));
        AttributeGroup color = productVariantService.createAttributeGroup(new VariantApiDtos.AttributeGroupCreateRequest("COLOR", "Color", 20));

        AttributeValue sizeS = productVariantService.createAttributeValue(size.getId(), new VariantApiDtos.AttributeValueCreateRequest("S", "S", 10));
        AttributeValue sizeM = productVariantService.createAttributeValue(size.getId(), new VariantApiDtos.AttributeValueCreateRequest("M", "M", 20));
        AttributeValue colorRed = productVariantService.createAttributeValue(color.getId(), new VariantApiDtos.AttributeValueCreateRequest("RED", "Red", 10));
        AttributeValue colorBlue = productVariantService.createAttributeValue(color.getId(), new VariantApiDtos.AttributeValueCreateRequest("BLUE", "Blue", 20));

        productVariantService.configureProductAttributes(product.getId(),
                new VariantApiDtos.ProductAttributeConfigRequest(
                        List.of(
                                new VariantApiDtos.ProductAttributeGroupSelection(size.getId(), 10, true),
                                new VariantApiDtos.ProductAttributeGroupSelection(color.getId(), 20, true)
                        ),
                        List.of(
                                new VariantApiDtos.ProductAttributeAllowedValues(size.getId(), List.of(sizeS.getId(), sizeM.getId())),
                                new VariantApiDtos.ProductAttributeAllowedValues(color.getId(), List.of(colorRed.getId(), colorBlue.getId()))
                        )
                )
        );

        VariantApiDtos.VariantGenerationResult first = productVariantService.generateVariants(product.getId(),
                new VariantApiDtos.VariantGenerateRequest("MERGE", null, null, 100));
        assertThat(first.created()).isEqualTo(4);

        String redSKey = keyService.canonicalKey(Map.of("SIZE", "S", "COLOR", "RED"));
        String redSHash = keyService.hash(redSKey);
        ProductVariant redS = productVariantRepo.findByProductAndCombinationHash(product, redSHash).orElseThrow();
        redS.setSku("DRK-S-RED");
        redS.setPrice(new BigDecimal("2.20"));
        redS.setEnabled(false);
        productVariantRepo.save(redS);

        productVariantService.configureProductAttributes(product.getId(),
                new VariantApiDtos.ProductAttributeConfigRequest(
                        List.of(
                                new VariantApiDtos.ProductAttributeGroupSelection(size.getId(), 10, true),
                                new VariantApiDtos.ProductAttributeGroupSelection(color.getId(), 20, true)
                        ),
                        List.of(
                                new VariantApiDtos.ProductAttributeAllowedValues(size.getId(), List.of(sizeS.getId(), sizeM.getId())),
                                new VariantApiDtos.ProductAttributeAllowedValues(color.getId(), List.of(colorRed.getId()))
                        )
                )
        );

        VariantApiDtos.VariantGenerationResult second = productVariantService.generateVariants(product.getId(),
                new VariantApiDtos.VariantGenerateRequest("MERGE", null, null, 100));

        assertThat(second.expectedVariants()).isEqualTo(2);
        assertThat(second.archived()).isEqualTo(2);

        ProductVariant mergedRedS = productVariantRepo.findByProductAndCombinationHash(product, redSHash).orElseThrow();
        assertThat(mergedRedS.getSku()).isEqualTo("DRK-S-RED");
        assertThat(mergedRedS.getPrice()).isEqualByComparingTo("2.20");
        assertThat(mergedRedS.getEnabled()).isFalse();
        assertThat(mergedRedS.getArchived()).isFalse();
    }

    @Test
    void pricingQuoteUsesCustomerGroupTierBeforeGlobalTierFor12Boxes() {
        Product product = new Product();
        product.setName("Spark Drink Variant Base");
        product = productRepo.save(product);

        ProductVariant variant = new ProductVariant();
        variant.setProduct(product);
        variant.setCombinationKey("SIZE=330ML|FLAVOR=COLA");
        variant.setCombinationHash(keyService.hash("SIZE=330ML|FLAVOR=COLA"));
        variant.setVariantName("330ML / Cola");
        variant.setEnabled(true);
        variant = productVariantRepo.save(variant);

        skuUnitPricingService.createUnit(new VariantApiDtos.UnitCreateRequest("PIECE", "Piece", 0));
        skuUnitPricingService.createUnit(new VariantApiDtos.UnitCreateRequest("BOX", "Box", 0));
        skuUnitPricingService.createCustomerGroup(new VariantApiDtos.CustomerGroupCreateRequest("WHOLESALE", "Wholesale", 10));

        SkuSellUnit box = skuUnitPricingService.upsertSellUnit(variant.getId(),
                new VariantApiDtos.SellUnitUpsertRequest("BOX", new BigDecimal("24"), false, new BigDecimal("34.00"), true));

        skuUnitPricingService.replaceTierPrices(box.getId(),
                new VariantApiDtos.TierPriceReplaceRequest("USD", List.of(
                        new VariantApiDtos.TierPriceItem("WHOLESALE", new BigDecimal("10"), new BigDecimal("30.50"), "USD", null, null, true),
                        new VariantApiDtos.TierPriceItem("WHOLESALE", new BigDecimal("20"), new BigDecimal("29.00"), "USD", null, null, true),
                        new VariantApiDtos.TierPriceItem(null, new BigDecimal("10"), new BigDecimal("32.00"), "USD", null, null, true),
                        new VariantApiDtos.TierPriceItem(null, new BigDecimal("20"), new BigDecimal("31.00"), "USD", null, null, true)
                ))
        );

        VariantApiDtos.PricingQuoteLineResponse line = pricingService.quoteLine(
                variant.getId(),
                box.getId(),
                new BigDecimal("12"),
                "WHOLESALE",
                "USD"
        );

        assertThat(line.priceSource()).isEqualTo("GROUP_TIER");
        assertThat(line.unitPrice()).isEqualByComparingTo("30.50");
        assertThat(line.lineSubtotal()).isEqualByComparingTo("366.00");
        assertThat(line.inventoryBaseQty()).isEqualByComparingTo("288.000000");
    }

    @Test
    void inventoryServiceConvertsSellUnitQtyToBaseAndDeductsStock() {
        Product product = new Product();
        product.setName("Spark Drink Inventory");
        product.setActive(true);
        product = productRepo.save(product);

        ProductVariant variant = new ProductVariant();
        variant.setProduct(product);
        variant.setCombinationKey("SIZE=500ML|FLAVOR=LEMON");
        variant.setCombinationHash(keyService.hash("SIZE=500ML|FLAVOR=LEMON"));
        variant.setVariantName("500ML / Lemon");
        variant.setEnabled(true);
        variant.setStockBaseQty(new BigDecimal("100.000000"));
        variant = productVariantRepo.save(variant);

        skuUnitPricingService.createUnit(new VariantApiDtos.UnitCreateRequest("BOX", "Box", 0));
        SkuSellUnit box = skuUnitPricingService.upsertSellUnit(variant.getId(),
                new VariantApiDtos.SellUnitUpsertRequest("BOX", new BigDecimal("24"), false, new BigDecimal("40.00"), true));

        InventoryService.VariantUnitDeductionResult result =
                inventoryService.deductVariantUnitStock(box.getId(), new BigDecimal("2"));

        assertThat(result.variantId()).isEqualTo(variant.getId());
        assertThat(result.sellUnitId()).isEqualTo(box.getId());
        assertThat(result.soldQty()).isEqualByComparingTo("2.000000");
        assertThat(result.deductedBaseQty()).isEqualByComparingTo("48.000000");
        assertThat(result.remainingBaseQty()).isEqualByComparingTo("52.000000");

        ProductVariant refreshed = productVariantRepo.findById(variant.getId()).orElseThrow();
        assertThat(refreshed.getStockBaseQty()).isEqualByComparingTo("52.000000");
    }
}
