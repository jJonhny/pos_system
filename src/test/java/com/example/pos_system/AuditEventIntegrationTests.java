package com.example.pos_system;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.entity.AppUser;
import com.example.pos_system.entity.Category;
import com.example.pos_system.entity.PaymentMethod;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.SalePayment;
import com.example.pos_system.entity.Shift;
import com.example.pos_system.entity.ShiftStatus;
import com.example.pos_system.entity.UserRole;
import com.example.pos_system.repository.AppUserRepo;
import com.example.pos_system.repository.AuditEventRepo;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.SaleRepo;
import com.example.pos_system.repository.ShiftRepo;
import com.example.pos_system.service.CheckoutAttemptService;
import com.example.pos_system.service.InventoryService;
import com.example.pos_system.service.PosService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

@SpringBootTest
@ActiveProfiles("test")
@Transactional
class AuditEventIntegrationTests {

    @Autowired
    private PosService posService;
    @Autowired
    private InventoryService inventoryService;
    @Autowired
    private AuditEventRepo auditEventRepo;
    @Autowired
    private ProductRepo productRepo;
    @Autowired
    private CategoryRepo categoryRepo;
    @Autowired
    private ShiftRepo shiftRepo;
    @Autowired
    private SaleRepo saleRepo;
    @Autowired
    private CheckoutAttemptService checkoutAttemptService;
    @Autowired
    private AppUserRepo appUserRepo;
    @Autowired
    private PasswordEncoder passwordEncoder;

    @Test
    @WithMockUser(username = "audit_admin", roles = "ADMIN")
    void checkoutIdempotencyCreatesSingleSaleAndSingleAuditEvent() {
        ensureActor("audit_admin");
        Product product = createProduct("AUD-CHECKOUT-01", 1);
        String terminalId = "TERM-AUDIT-01";
        Shift shift = createShift("audit_admin", terminalId);
        long saleCountBefore = saleRepo.count();
        AtomicInteger operationCalls = new AtomicInteger();
        String clientCheckoutId = UUID.randomUUID().toString();

        CheckoutAttemptService.CheckoutResult first = checkoutAttemptService.process(
                clientCheckoutId,
                terminalId,
                () -> {
                    operationCalls.incrementAndGet();
                    Cart cart = new Cart();
                    cart.add(productRepo.findById(product.getId()).orElseThrow());
                    SalePayment payment = new SalePayment();
                    payment.setMethod(PaymentMethod.CASH);
                    return posService.checkout(cart, payment, "audit_admin", null, shift, terminalId);
                }
        );

        CheckoutAttemptService.CheckoutResult second = checkoutAttemptService.process(
                clientCheckoutId,
                terminalId,
                () -> {
                    operationCalls.incrementAndGet();
                    throw new IllegalStateException("Idempotent replay should not execute checkout again.");
                }
        );

        assertThat(operationCalls.get()).isEqualTo(1);
        assertThat(second.replayed()).isTrue();
        assertThat(first.sale().getId()).isEqualTo(second.sale().getId());
        assertThat(saleRepo.count()).isEqualTo(saleCountBefore + 1);
        assertThat(auditEventRepo.countByActionType("POS_CHECKOUT")).isEqualTo(1);

        var event = auditEventRepo.findTopByActionTypeOrderByTimestampDesc("POS_CHECKOUT").orElse(null);
        assertThat(event).isNotNull();
        assertThat(event.getActorUsername()).isEqualTo("audit_admin");
        assertThat(event.getActorUserId()).isNotNull();
        assertThat(event.getTargetType()).isEqualTo("SALE");
        assertThat(event.getAfterJson()).contains("paymentMethod");
        assertThat(event.getBeforeJson()).contains("items");
    }

    @Test
    @WithMockUser(username = "audit_admin", roles = "ADMIN")
    void stockAdjustmentAuditPreventsNegativeInventory() {
        ensureActor("audit_admin");
        Product product = createProduct("AUD-STOCK-01", 3);

        assertThatThrownBy(() -> inventoryService.bulkAdjustStock(List.of(product.getId()), "remove", "10"))
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("Insufficient stock");

        Product updated = productRepo.findById(product.getId()).orElseThrow();
        assertThat(updated.getStockQty()).isEqualTo(3);
    }

    private Product createProduct(String sku, int stockQty) {
        Category category = new Category();
        category.setName("Audit Category " + sku);
        category.setActive(true);
        category.setSortOrder(1);
        Category savedCategory = categoryRepo.save(category);

        Product product = new Product();
        product.setName("Audit Product " + sku);
        product.setSku(sku);
        product.setPrice(new BigDecimal("9.99"));
        product.setStockQty(stockQty);
        product.setActive(true);
        product.setCategory(savedCategory);
        return productRepo.save(product);
    }

    private Shift createShift(String username, String terminalId) {
        Shift shift = new Shift();
        shift.setCashierUsername(username);
        shift.setTerminalId(terminalId);
        shift.setOpenedAt(LocalDateTime.now());
        shift.setStatus(ShiftStatus.OPEN);
        shift.setOpeningCash(new BigDecimal("10.00"));
        return shiftRepo.save(shift);
    }

    private void ensureActor(String username) {
        if (appUserRepo.findByUsername(username).isPresent()) return;
        AppUser user = new AppUser();
        user.setUsername(username);
        user.setPassword(passwordEncoder.encode("password"));
        user.setRole(UserRole.ADMIN);
        user.setActive(true);
        appUserRepo.save(user);
    }
}
