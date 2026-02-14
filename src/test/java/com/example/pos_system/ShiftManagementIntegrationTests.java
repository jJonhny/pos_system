package com.example.pos_system;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.entity.AppUser;
import com.example.pos_system.entity.Category;
import com.example.pos_system.entity.PaymentMethod;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SalePayment;
import com.example.pos_system.entity.Shift;
import com.example.pos_system.entity.ShiftCashEventType;
import com.example.pos_system.entity.ShiftStatus;
import com.example.pos_system.entity.UserRole;
import com.example.pos_system.repository.AppUserRepo;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.SaleRepo;
import com.example.pos_system.service.PosService;
import com.example.pos_system.service.ShiftService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@ActiveProfiles("test")
@Transactional
class ShiftManagementIntegrationTests {
    @Autowired
    private ShiftService shiftService;
    @Autowired
    private PosService posService;
    @Autowired
    private SaleRepo saleRepo;
    @Autowired
    private CategoryRepo categoryRepo;
    @Autowired
    private ProductRepo productRepo;
    @Autowired
    private AppUserRepo appUserRepo;
    @Autowired
    private PasswordEncoder passwordEncoder;

    @Test
    @WithMockUser(username = "shift_manager", roles = "MANAGER")
    void openSaleCloseShiftReconciliationWorks() {
        ensureUser("shift_manager", UserRole.MANAGER);
        Product product = createProduct("SHIFT-OPEN-CLOSE-01", 50, new BigDecimal("10.00"));

        Shift opened = shiftService.openShift(
                "shift_manager",
                "TERM-A1",
                Map.of("USD", new BigDecimal("100.00"))
        );
        assertThat(opened.getStatus()).isEqualTo(ShiftStatus.OPEN);

        Cart cart = new Cart();
        cart.add(product);

        SalePayment payment = new SalePayment();
        payment.setMethod(PaymentMethod.CASH);
        payment.setCurrencyCode("USD");
        payment.setCurrencyRate(BigDecimal.ONE);
        payment.setForeignAmount(new BigDecimal("10.00"));
        payment.setAmount(new BigDecimal("10.00"));

        Sale sale = posService.checkout(cart, payment, "shift_manager", null, opened, "TERM-A1");
        assertThat(sale.getShift()).isNotNull();
        assertThat(sale.getTerminalId()).isEqualTo("TERM-A1");

        shiftService.addCashEvent(
                "shift_manager",
                "TERM-A1",
                ShiftCashEventType.CASH_IN,
                "USD",
                new BigDecimal("5.00"),
                "Float top-up"
        );

        ShiftService.ShiftCloseResult closeResult = shiftService.closeShift(
                "shift_manager",
                "TERM-A1",
                Map.of("USD", new BigDecimal("115.00")),
                "All cash counted.",
                true
        );

        Shift closed = closeResult.shift();
        assertThat(closed.getStatus()).isEqualTo(ShiftStatus.CLOSED);
        assertThat(closed.getExpectedCash()).isEqualByComparingTo("115.00");
        assertThat(closed.getClosingCash()).isEqualByComparingTo("115.00");
        assertThat(closed.getVarianceCash()).isEqualByComparingTo("0.00");

        Sale persistedSale = saleRepo.findById(sale.getId()).orElseThrow();
        assertThat(persistedSale.getShift()).isNotNull();
        assertThat(persistedSale.getShift().getId()).isEqualTo(closed.getId());
        assertThat(persistedSale.getTerminalId()).isEqualTo("TERM-A1");
    }

    private Product createProduct(String sku, int stockQty, BigDecimal price) {
        Category category = new Category();
        category.setName("Shift Category " + sku);
        category.setActive(true);
        category.setSortOrder(1);
        Category savedCategory = categoryRepo.save(category);

        Product product = new Product();
        product.setName("Shift Product " + sku);
        product.setSku(sku);
        product.setPrice(price);
        product.setStockQty(stockQty);
        product.setActive(true);
        product.setCategory(savedCategory);
        return productRepo.save(product);
    }

    private void ensureUser(String username, UserRole role) {
        if (appUserRepo.findByUsername(username).isPresent()) return;
        AppUser user = new AppUser();
        user.setUsername(username);
        user.setPassword(passwordEncoder.encode("password"));
        user.setRole(role);
        user.setActive(true);
        appUserRepo.save(user);
    }
}
