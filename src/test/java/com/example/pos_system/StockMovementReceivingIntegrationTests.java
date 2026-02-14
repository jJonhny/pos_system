package com.example.pos_system;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.entity.Category;
import com.example.pos_system.entity.PaymentMethod;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.PurchaseOrder;
import com.example.pos_system.entity.PurchaseOrderStatus;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SalePayment;
import com.example.pos_system.entity.Shift;
import com.example.pos_system.entity.StockMovement;
import com.example.pos_system.entity.StockMovementType;
import com.example.pos_system.entity.Supplier;
import com.example.pos_system.entity.SupplierStatus;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.StockMovementRepo;
import com.example.pos_system.repository.SupplierRepo;
import com.example.pos_system.service.PosService;
import com.example.pos_system.service.PurchaseService;
import com.example.pos_system.service.SalesService;
import com.example.pos_system.service.ShiftService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

@SpringBootTest
@ActiveProfiles("test")
@Transactional
class StockMovementReceivingIntegrationTests {

    @Autowired
    private PurchaseService purchaseService;
    @Autowired
    private PosService posService;
    @Autowired
    private SalesService salesService;
    @Autowired
    private ShiftService shiftService;
    @Autowired
    private ProductRepo productRepo;
    @Autowired
    private CategoryRepo categoryRepo;
    @Autowired
    private SupplierRepo supplierRepo;
    @Autowired
    private StockMovementRepo stockMovementRepo;

    @Test
    @WithMockUser(username = "ops_manager", roles = "MANAGER")
    void grnPostingIncreasesStockAndWritesReceiveMovement() {
        Product product = createProduct("GRN-STOCK-01", 5, new BigDecimal("2.50"));
        Supplier supplier = createSupplier("Global Supplies");

        PurchaseOrder po = purchaseService.savePurchaseOrder(
                null,
                supplier.getId(),
                PurchaseOrderStatus.SENT,
                "USD",
                LocalDate.now().plusDays(2),
                "PO for receive test",
                List.of(new PurchaseService.PurchaseOrderLineInput(product.getId(), 6, new BigDecimal("2.50"), null, null))
        );

        purchaseService.postGoodsReceipt(
                po.getId(),
                "INV-1001",
                "Partial receipt",
                "TERM-RCV-01",
                List.of(new PurchaseService.GoodsReceiptLineInput(product.getId(), 3, new BigDecimal("2.50")))
        );

        Product updated = productRepo.findById(product.getId()).orElseThrow();
        assertThat(updated.getStockQty()).isEqualTo(8);

        List<StockMovement> receiveMovements = stockMovementRepo.findAll().stream()
                .filter(m -> m.getType() == StockMovementType.RECEIVE)
                .toList();
        assertThat(receiveMovements).hasSize(1);
        assertThat(receiveMovements.getFirst().getQtyDelta()).isEqualTo(3);

        PurchaseOrder refreshedPo = purchaseService.getPurchaseOrder(po.getId());
        assertThat(refreshedPo.getStatus()).isEqualTo(PurchaseOrderStatus.PARTIAL);
    }

    @Test
    @WithMockUser(username = "ops_manager", roles = "MANAGER")
    void checkoutDecreasesStockAndWritesSaleMovement() {
        Product product = createProduct("SALE-STOCK-01", 2, new BigDecimal("9.99"));
        Shift shift = shiftService.openShift("ops_manager", "TERM-SALE-01", Map.of("USD", new BigDecimal("50.00")));

        Cart cart = new Cart();
        cart.add(product);

        SalePayment payment = new SalePayment();
        payment.setMethod(PaymentMethod.CASH);
        payment.setAmount(new BigDecimal("9.99"));

        Sale sale = posService.checkout(cart, payment, "ops_manager", null, shift, "TERM-SALE-01");

        Product updated = productRepo.findById(product.getId()).orElseThrow();
        assertThat(updated.getStockQty()).isEqualTo(1);

        List<StockMovement> saleMovements = stockMovementRepo.findAll().stream()
                .filter(m -> m.getType() == StockMovementType.SALE)
                .toList();
        assertThat(saleMovements).hasSize(1);
        assertThat(saleMovements.getFirst().getQtyDelta()).isEqualTo(-1);
        assertThat(saleMovements.getFirst().getRefId()).isEqualTo(String.valueOf(sale.getId()));
    }

    @Test
    @WithMockUser(username = "ops_manager", roles = "MANAGER")
    void returnIncreasesStockAndWritesReturnMovement() {
        Product product = createProduct("RETURN-STOCK-01", 2, new BigDecimal("8.50"));
        Shift shift = shiftService.openShift("ops_manager", "TERM-RET-01", Map.of("USD", new BigDecimal("20.00")));

        Cart cart = new Cart();
        cart.add(product);
        SalePayment payment = new SalePayment();
        payment.setMethod(PaymentMethod.CASH);
        payment.setAmount(new BigDecimal("8.50"));
        Sale sale = posService.checkout(cart, payment, "ops_manager", null, shift, "TERM-RET-01");

        String returnKey = "returnQty_" + sale.getItems().getFirst().getId();
        salesService.processReturn(sale.getId(), Map.of(returnKey, "1"));

        Product updated = productRepo.findById(product.getId()).orElseThrow();
        assertThat(updated.getStockQty()).isEqualTo(2);

        List<StockMovement> returnMovements = stockMovementRepo.findAll().stream()
                .filter(m -> m.getType() == StockMovementType.RETURN)
                .toList();
        assertThat(returnMovements).hasSize(1);
        assertThat(returnMovements.getFirst().getQtyDelta()).isEqualTo(1);
        assertThat(returnMovements.getFirst().getRefId()).isEqualTo(String.valueOf(sale.getId()));
    }

    @Test
    @WithMockUser(username = "ops_manager", roles = "MANAGER")
    void negativeStockBlockedWhenAllowNegativeStockFalse() {
        Product product = createProduct("NEG-STOCK-01", 1, new BigDecimal("5.00"));
        product.setAllowNegativeStock(false);
        productRepo.save(product);

        Shift shift = shiftService.openShift("ops_manager", "TERM-NEG-01", Map.of("USD", new BigDecimal("20.00")));

        Cart cart = new Cart();
        cart.add(product);
        cart.setQty(product.getId(), 2);

        SalePayment payment = new SalePayment();
        payment.setMethod(PaymentMethod.CASH);
        payment.setAmount(new BigDecimal("10.00"));

        assertThatThrownBy(() -> posService.checkout(cart, payment, "ops_manager", null, shift, "TERM-NEG-01"))
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("Insufficient stock");

        Product updated = productRepo.findById(product.getId()).orElseThrow();
        assertThat(updated.getStockQty()).isEqualTo(1);

        long saleMovementCount = stockMovementRepo.findAll().stream()
                .filter(m -> m.getType() == StockMovementType.SALE)
                .count();
        assertThat(saleMovementCount).isZero();
    }

    private Product createProduct(String sku, int stockQty, BigDecimal price) {
        Category category = new Category();
        category.setName("Inventory Category " + sku);
        category.setActive(true);
        category.setSortOrder(1);
        Category savedCategory = categoryRepo.save(category);

        Product product = new Product();
        product.setName("Inventory Product " + sku);
        product.setSku(sku);
        product.setPrice(price);
        product.setCostPrice(price.divide(new BigDecimal("2"), 2, java.math.RoundingMode.HALF_UP));
        product.setStockQty(stockQty);
        product.setActive(true);
        product.setAllowNegativeStock(false);
        product.setCategory(savedCategory);
        return productRepo.save(product);
    }

    private Supplier createSupplier(String name) {
        Supplier supplier = new Supplier();
        supplier.setName(name);
        supplier.setStatus(SupplierStatus.ACTIVE);
        return supplierRepo.save(supplier);
    }
}
