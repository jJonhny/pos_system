package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.StockMovement;
import com.devcore.pos_system.repository.ProductRepo;
import com.devcore.pos_system.repository.StockMovementRepo;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.ActiveProfiles;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@SpringBootTest
@ActiveProfiles("test")
class StockMovementServiceQtyBaseTest {
    @Autowired
    private StockMovementService stockMovementService;
    @Autowired
    private ProductRepo productRepo;
    @Autowired
    private StockMovementRepo stockMovementRepo;

    @Test
    void recordReceiveAndSaleShouldPersistQtyBaseAndUnitId() {
        Product product = new Product();
        product.setName("Stock Product");
        product.setSku("stock-test-1");
        product.setPrice(new BigDecimal("2.00"));
        product.setStockQty(10);
        product.setAllowNegativeStock(false);
        Product savedProduct = productRepo.save(product);

        stockMovementService.recordReceive(
                savedProduct.getId(),
                24,
                9L,
                new BigDecimal("1.5000"),
                "usd",
                "GRN",
                "88",
                "POS-01",
                "receive"
        );
        stockMovementService.recordSale(
                savedProduct.getId(),
                4,
                9L,
                new BigDecimal("2.0000"),
                "usd",
                "SALE",
                "99",
                "POS-01",
                "sale"
        );

        Product reloaded = productRepo.findById(savedProduct.getId()).orElseThrow();
        assertEquals(30, reloaded.getStockQty());

        List<StockMovement> movements = stockMovementRepo.findAll(Sort.by(Sort.Direction.ASC, "id"));
        StockMovement receive = movements.get(movements.size() - 2);
        StockMovement sale = movements.get(movements.size() - 1);

        assertEquals(24, receive.getQtyDelta());
        assertEquals(24, receive.getQtyBase());
        assertEquals(9L, receive.getUnitId());

        assertEquals(-4, sale.getQtyDelta());
        assertEquals(-4, sale.getQtyBase());
        assertEquals(9L, sale.getUnitId());
    }
}
