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
import com.example.pos_system.repository.ShiftRepo;
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

import static org.assertj.core.api.Assertions.assertThat;

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
    private AppUserRepo appUserRepo;
    @Autowired
    private PasswordEncoder passwordEncoder;

    @Test
    @WithMockUser(username = "audit_admin", roles = "ADMIN")
    void checkoutWritesAuditEvent() {
        ensureActor("audit_admin");
        Product product = createProduct("AUD-CHECKOUT-01", 25);
        Shift shift = createShift("audit_admin");

        Cart cart = new Cart();
        cart.add(product);

        SalePayment payment = new SalePayment();
        payment.setMethod(PaymentMethod.CASH);
        posService.checkout(cart, payment, "audit_admin", null, shift);

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
    void stockAdjustmentWritesAuditEvent() {
        ensureActor("audit_admin");
        Product product = createProduct("AUD-STOCK-01", 7);

        inventoryService.quickUpdate(product.getId(), null, "12");

        Product updated = productRepo.findById(product.getId()).orElseThrow();
        assertThat(updated.getStockQty()).isEqualTo(12);

        var event = auditEventRepo.findTopByActionTypeOrderByTimestampDesc("STOCK_ADJUSTMENT").orElse(null);
        assertThat(event).isNotNull();
        assertThat(event.getActorUsername()).isEqualTo("audit_admin");
        assertThat(event.getTargetType()).isEqualTo("PRODUCT");
        assertThat(event.getTargetId()).isEqualTo(String.valueOf(product.getId()));
        assertThat(event.getAfterJson()).contains("\"stockQty\":12");
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

    private Shift createShift(String username) {
        Shift shift = new Shift();
        shift.setCashierUsername(username);
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
