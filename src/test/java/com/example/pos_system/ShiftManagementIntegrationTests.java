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
import com.example.pos_system.service.SalesService;
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
import static org.assertj.core.api.Assertions.assertThatThrownBy;

@SpringBootTest
@ActiveProfiles("test")
@Transactional
class ShiftManagementIntegrationTests {
    @Autowired
    private ShiftService shiftService;
    @Autowired
    private PosService posService;
    @Autowired
    private SalesService salesService;
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

    /**
     * Executes the openSaleCloseShiftReconciliationWorks operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the openSaleCloseShiftReconciliationWorks operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the openSaleCloseShiftReconciliationWorks operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

        String returnKey = "returnQty_" + sale.getItems().get(0).getId();
        salesService.processReturn(sale.getId(), Map.of(returnKey, "1"));

        shiftService.addCashEvent(
                "shift_manager",
                "TERM-A1",
                ShiftCashEventType.CASH_OUT,
                "USD",
                new BigDecimal("3.00"),
                "Petty cash payout"
        );

        ShiftService.ShiftCloseResult closeResult = shiftService.closeShift(
                "shift_manager",
                "TERM-A1",
                Map.of("USD", new BigDecimal("97.00")),
                "All cash counted.",
                true
        );

        Shift closed = closeResult.shift();
        assertThat(closed.getStatus()).isEqualTo(ShiftStatus.CLOSED);
        assertThat(closed.getExpectedCash()).isEqualByComparingTo("97.00");
        assertThat(closed.getCashRefundTotal()).isEqualByComparingTo("10.00");
        assertThat(closed.getCashOutTotal()).isEqualByComparingTo("3.00");
        assertThat(closed.getClosingCash()).isEqualByComparingTo("97.00");
        assertThat(closed.getVarianceCash()).isEqualByComparingTo("0.00");

        Sale persistedSale = saleRepo.findById(sale.getId()).orElseThrow();
        assertThat(persistedSale.getShift()).isNotNull();
        assertThat(persistedSale.getShift().getId()).isEqualTo(closed.getId());
        assertThat(persistedSale.getTerminalId()).isEqualTo("TERM-A1");
    }

    /**
     * Executes the closeShiftWithVarianceOverThresholdRequiresManagerApproval operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the closeShiftWithVarianceOverThresholdRequiresManagerApproval operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the closeShiftWithVarianceOverThresholdRequiresManagerApproval operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    @WithMockUser(username = "shift_cashier", roles = "CASHIER")
    void closeShiftWithVarianceOverThresholdRequiresManagerApproval() {
        ensureUser("shift_cashier", UserRole.CASHIER);
        shiftService.openShift(
                "shift_cashier",
                "TERM-B1",
                Map.of("USD", new BigDecimal("50.00"))
        );

        assertThatThrownBy(() -> shiftService.closeShift(
                "shift_cashier",
                "TERM-B1",
                Map.of("USD", new BigDecimal("95.00")),
                "Variance check",
                false
        ))
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("Manager approval required");
    }

    /**
     * Executes the createProduct operation.
     *
     * @param sku Parameter of type {@code String} used by this operation.
     * @param stockQty Parameter of type {@code int} used by this operation.
     * @param price Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the ensureUser operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
