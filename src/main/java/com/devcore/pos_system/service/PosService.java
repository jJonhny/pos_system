package com.devcore.pos_system.service;

import com.devcore.pos_system.dto.Cart;
import com.devcore.pos_system.dto.CartItem;
import com.devcore.pos_system.entity.*;
import com.devcore.pos_system.repository.CustomerRepo;
import com.devcore.pos_system.repository.DiscountAuditRepo;
import com.devcore.pos_system.repository.ProductRepo;
import com.devcore.pos_system.repository.SaleRepo;
import org.springframework.stereotype.Service;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Service
@Transactional
public class PosService {

    private final ProductRepo productRepo;
    private final SaleRepo saleRepo;
    private final CustomerRepo customerRepo;
    private final DiscountAuditRepo discountAuditRepo;
    private final AuditEventService auditEventService;
    private final StockMovementService stockMovementService;
    private final VariantInventoryService variantInventoryService;
    private final UserLocalePreferenceService userLocalePreferenceService;
    private final I18nService i18nService;
    private final MarketingPricingService marketingPricingService;

    /**
     * Executes the PosService operation.
     * <p>Return value: A fully initialized PosService instance.</p>
     *
     * @param productRepo Parameter of type {@code ProductRepo} used by this operation.
     * @param saleRepo Parameter of type {@code SaleRepo} used by this operation.
     * @param customerRepo Parameter of type {@code CustomerRepo} used by this operation.
     * @param discountAuditRepo Parameter of type {@code DiscountAuditRepo} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * @param stockMovementService Parameter of type {@code StockMovementService} used by this operation.
     * @param variantInventoryService Parameter of type {@code VariantInventoryService} used by this operation.
     * @param userLocalePreferenceService Parameter of type {@code UserLocalePreferenceService} used by this operation.
     * @param i18nService Parameter of type {@code I18nService} used by this operation.
     * @param marketingPricingService Parameter of type {@code MarketingPricingService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public PosService(ProductRepo productRepo, SaleRepo saleRepo, CustomerRepo customerRepo,
                      DiscountAuditRepo discountAuditRepo, AuditEventService auditEventService,
                      StockMovementService stockMovementService,
                      VariantInventoryService variantInventoryService,
                      UserLocalePreferenceService userLocalePreferenceService,
                      I18nService i18nService,
                      MarketingPricingService marketingPricingService) {
        this.productRepo = productRepo;
        this.saleRepo = saleRepo;
        this.customerRepo = customerRepo;
        this.discountAuditRepo = discountAuditRepo;
        this.auditEventService = auditEventService;
        this.stockMovementService = stockMovementService;
        this.variantInventoryService = variantInventoryService;
        this.userLocalePreferenceService = userLocalePreferenceService;
        this.i18nService = i18nService;
        this.marketingPricingService = marketingPricingService;
    }

    /**
     * Executes the checkout operation.
     *
     * @param cart Parameter of type {@code Cart} used by this operation.
     * @param payment Parameter of type {@code SalePayment} used by this operation.
     * @param cashierUsername Parameter of type {@code String} used by this operation.
     * @param customer Parameter of type {@code Customer} used by this operation.
     * @param shift Parameter of type {@code Shift} used by this operation.
     * @return {@code Sale} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Sale checkout(Cart cart, SalePayment payment, String cashierUsername, Customer customer, Shift shift) {
        return checkout(cart, payment, cashierUsername, customer, shift, null);
    }

    /**
     * Executes the checkout operation.
     *
     * @param cart Parameter of type {@code Cart} used by this operation.
     * @param payment Parameter of type {@code SalePayment} used by this operation.
     * @param cashierUsername Parameter of type {@code String} used by this operation.
     * @param customer Parameter of type {@code Customer} used by this operation.
     * @param shift Parameter of type {@code Shift} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code Sale} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Sale checkout(Cart cart, SalePayment payment, String cashierUsername, Customer customer, Shift shift, String terminalId) {
        if (cart.getItems().isEmpty()) throw new IllegalStateException(msg("pos.error.cartEmpty"));
        marketingPricingService.applyBestCampaign(cart, customer);
        String checkoutTerminalId = requireCheckoutShift(shift, terminalId);

        PaymentMethod method = payment == null ? PaymentMethod.CASH : payment.getMethod();
        Sale sale = new Sale();
        sale.setCreatedAt(LocalDateTime.now());
        sale.setPaymentMethod(method);
        sale.setStatus(SaleStatus.PAID);
        sale.setReceiptLocale(userLocalePreferenceService.toLanguageTag(LocaleContextHolder.getLocale()));
        sale.setCashierUsername(cashierUsername);
        sale.setTerminalId(checkoutTerminalId);
        sale.setCustomer(customer);
        sale.setShift(shift);

        sale.setSubtotal(cart.getSubtotal());
        sale.setDiscount(cart.getDiscount());
        sale.setDiscountType(cart.getDiscountType());
        sale.setDiscountValue(cart.getDiscountValue());
        sale.setDiscountReason(cart.getDiscountReason());
        sale.setDiscountAppliedBy(cashierUsername);
        sale.setTax(cart.getTax());
        sale.setTotal(cart.getTotal());
        sale.setRefundedTotal(BigDecimal.ZERO);
        sale = saleRepo.save(sale);

        for (CartItem ci : cart.getItems()) {
            Product p = consumeCartItemInventory(ci, sale, checkoutTerminalId, "POS checkout");
            SaleItem si = new SaleItem();
            si.setSale(sale);
            si.setProduct(p);
            si.setQty(ci.getQty());
            si.setUnitPrice(ci.getUnitPrice());
            si.setLineTotal(ci.getLineTotal());
            si.setNote(ci.getNote());
            si.setPriceTier(ci.getPriceTier());
            si.setUnitType(ci.getUnitType());
            si.setUnitSize(ci.getUnitSize());
            si.setUnitId(ci.getUnitId());
            si.setQtyBase(ci.getEffectiveBaseQty().setScale(0, RoundingMode.HALF_UP).intValue());
            applyVariantLineFields(si, ci);
            si.setReturnedQty(0);

            sale.getItems().add(si);
        }

        if (payment == null) {
            payment = new SalePayment();
            payment.setMethod(method);
            payment.setAmount(sale.getTotal());
        }
        payment.setSale(sale);
        if (payment.getAmount() == null) {
            payment.setAmount(sale.getTotal());
        }
        sale.getPayments().add(payment);

        applyLoyaltyPoints(sale, customer);
        Sale saved = saleRepo.save(sale);
        recordDiscountAudit(saved, cart, cashierUsername);
        recordCheckoutAudit("POS_CHECKOUT", saved, cart);
        return saved;
    }

    /**
     * Executes the checkoutSplit operation.
     *
     * @param cart Parameter of type {@code Cart} used by this operation.
     * @param payments Parameter of type {@code List<SalePayment>} used by this operation.
     * @param cashierUsername Parameter of type {@code String} used by this operation.
     * @param customer Parameter of type {@code Customer} used by this operation.
     * @param shift Parameter of type {@code Shift} used by this operation.
     * @return {@code Sale} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Sale checkoutSplit(Cart cart, List<SalePayment> payments, String cashierUsername, Customer customer, Shift shift) {
        return checkoutSplit(cart, payments, cashierUsername, customer, shift, null);
    }

    /**
     * Executes the checkoutSplit operation.
     *
     * @param cart Parameter of type {@code Cart} used by this operation.
     * @param payments Parameter of type {@code List<SalePayment>} used by this operation.
     * @param cashierUsername Parameter of type {@code String} used by this operation.
     * @param customer Parameter of type {@code Customer} used by this operation.
     * @param shift Parameter of type {@code Shift} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code Sale} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Sale checkoutSplit(Cart cart, List<SalePayment> payments, String cashierUsername, Customer customer, Shift shift, String terminalId) {
        if (cart.getItems().isEmpty()) throw new IllegalStateException(msg("pos.error.cartEmpty"));
        if (payments == null || payments.isEmpty()) throw new IllegalStateException(msg("pos.error.noPayments"));
        marketingPricingService.applyBestCampaign(cart, customer);
        String checkoutTerminalId = requireCheckoutShift(shift, terminalId);

        Sale sale = new Sale();
        sale.setCreatedAt(LocalDateTime.now());
        sale.setPaymentMethod(PaymentMethod.MIXED);
        sale.setStatus(SaleStatus.PAID);
        sale.setReceiptLocale(userLocalePreferenceService.toLanguageTag(LocaleContextHolder.getLocale()));
        sale.setCashierUsername(cashierUsername);
        sale.setTerminalId(checkoutTerminalId);
        sale.setCustomer(customer);
        sale.setShift(shift);

        sale.setSubtotal(cart.getSubtotal());
        sale.setDiscount(cart.getDiscount());
        sale.setDiscountType(cart.getDiscountType());
        sale.setDiscountValue(cart.getDiscountValue());
        sale.setDiscountReason(cart.getDiscountReason());
        sale.setDiscountAppliedBy(cashierUsername);
        sale.setTax(cart.getTax());
        sale.setTotal(cart.getTotal());
        sale.setRefundedTotal(BigDecimal.ZERO);
        sale = saleRepo.save(sale);

        for (CartItem ci : cart.getItems()) {
            Product p = consumeCartItemInventory(ci, sale, checkoutTerminalId, "POS split checkout");
            SaleItem si = new SaleItem();
            si.setSale(sale);
            si.setProduct(p);
            si.setQty(ci.getQty());
            si.setUnitPrice(ci.getUnitPrice());
            si.setLineTotal(ci.getLineTotal());
            si.setNote(ci.getNote());
            si.setPriceTier(ci.getPriceTier());
            si.setUnitType(ci.getUnitType());
            si.setUnitSize(ci.getUnitSize());
            si.setUnitId(ci.getUnitId());
            si.setQtyBase(ci.getEffectiveBaseQty().setScale(0, RoundingMode.HALF_UP).intValue());
            applyVariantLineFields(si, ci);
            si.setReturnedQty(0);

            sale.getItems().add(si);
        }

        for (SalePayment payment : payments) {
            payment.setSale(sale);
            sale.getPayments().add(payment);
        }

        applyLoyaltyPoints(sale, customer);
        Sale saved = saleRepo.save(sale);
        recordDiscountAudit(saved, cart, cashierUsername);
        recordCheckoutAudit("POS_CHECKOUT_SPLIT", saved, cart);
        return saved;
    }

    /**
     * Executes the consumeCartItemInventory operation.
     *
     * @param item Parameter of type {@code CartItem} used by this operation.
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Product consumeCartItemInventory(CartItem item, Sale sale, String terminalId, String notes) {
        if (item == null) {
            throw new IllegalArgumentException(msg("pos.error.productNotFound"));
        }
        if (Boolean.TRUE.equals(item.isVariantLine())) {
            ProductVariant variant = variantInventoryService.recordSale(item.getVariantId(), item.getEffectiveBaseQty());
            Product product = variant.getProduct();
            if (product == null) {
                throw new IllegalStateException(msg("pos.error.productNotFound"));
            }
            return product;
        }
        return stockMovementService.recordSale(
                item.getProductId(),
                item.getEffectiveBaseQty().setScale(0, RoundingMode.HALF_UP).intValue(),
                item.getUnitId(),
                null,
                null,
                "SALE",
                String.valueOf(sale.getId()),
                terminalId,
                notes
        );
    }

    /**
     * Executes the applyVariantLineFields operation.
     *
     * @param saleItem Parameter of type {@code SaleItem} used by this operation.
     * @param item Parameter of type {@code CartItem} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void applyVariantLineFields(SaleItem saleItem, CartItem item) {
        if (saleItem == null || item == null || !item.isVariantLine()) return;
        saleItem.setVariantId(item.getVariantId());
        saleItem.setUnitId(item.getUnitId());
        saleItem.setSellUnitId(item.getSellUnitId());
        saleItem.setSellUnitCode(item.getSellUnitCode());
        saleItem.setConversionToBase(item.getConversionToBase());
        saleItem.setPriceSource(item.getPriceSource());
        saleItem.setAppliedTierMinQty(item.getAppliedTierMinQty());
        saleItem.setAppliedTierGroupCode(item.getAppliedTierGroupCode());
    }

    /**
     * Executes the applyLoyaltyPoints operation.
     *
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @param customer Parameter of type {@code Customer} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void applyLoyaltyPoints(Sale sale, Customer customer) {
        if (customer == null) return;
        int points = sale.getTotal() == null ? 0 : sale.getTotal().setScale(0, RoundingMode.FLOOR).intValue();
        if (points < 0) points = 0;
        sale.setPointsEarned(points);
        Integer existing = customer.getPoints() == null ? 0 : customer.getPoints();
        customer.setPoints(existing + points);
        customerRepo.save(customer);
    }

    /**
     * Executes the recordDiscountAudit operation.
     *
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @param cart Parameter of type {@code Cart} used by this operation.
     * @param cashierUsername Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void recordDiscountAudit(Sale sale, Cart cart, String cashierUsername) {
        if (sale == null || cart == null) return;
        BigDecimal discountAmount = cart.getDiscount() == null ? BigDecimal.ZERO : cart.getDiscount();
        String reason = cart.getDiscountReason();
        if (discountAmount.compareTo(BigDecimal.ZERO) <= 0 && (reason == null || reason.isBlank())) {
            return;
        }
        BigDecimal subtotalBefore = cart.getSubtotal() == null ? BigDecimal.ZERO : cart.getSubtotal();
        BigDecimal subtotalAfter = subtotalBefore.subtract(discountAmount);
        if (subtotalAfter.compareTo(BigDecimal.ZERO) < 0) subtotalAfter = BigDecimal.ZERO;

        DiscountAudit audit = new DiscountAudit();
        audit.setCreatedAt(LocalDateTime.now());
        audit.setSale(sale);
        audit.setScope("CART");
        audit.setDiscountType(cart.getDiscountType());
        audit.setDiscountValue(cart.getDiscountValue());
        audit.setDiscountAmount(discountAmount);
        audit.setSubtotalBefore(subtotalBefore);
        audit.setSubtotalAfter(subtotalAfter);
        audit.setReason(reason);
        audit.setAppliedBy(cashierUsername);
        discountAuditRepo.save(audit);
    }

    /**
     * Executes the recordCheckoutAudit operation.
     *
     * @param actionType Parameter of type {@code String} used by this operation.
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @param cart Parameter of type {@code Cart} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void recordCheckoutAudit(String actionType, Sale sale, Cart cart) {
        if (sale == null || cart == null) return;
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("paymentCount", sale.getPayments() == null ? 0 : sale.getPayments().size());
        metadata.put("terminalId", sale.getTerminalId());
        metadata.put("shiftId", sale.getShift() == null ? null : sale.getShift().getId());
        auditEventService.record(
                actionType,
                "SALE",
                sale.getId(),
                cartSnapshot(cart),
                saleSnapshot(sale),
                metadata
        );
    }

    /**
     * Executes the cartSnapshot operation.
     *
     * @param cart Parameter of type {@code Cart} used by this operation.
     * @return {@code Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, Object> cartSnapshot(Cart cart) {
        Map<String, Object> snapshot = new LinkedHashMap<>();
        snapshot.put("subtotal", cart.getSubtotal());
        snapshot.put("discountType", cart.getDiscountType() == null ? null : cart.getDiscountType().name());
        snapshot.put("discountValue", cart.getDiscountValue());
        snapshot.put("discount", cart.getDiscount());
        snapshot.put("discountReason", cart.getDiscountReason());
        snapshot.put("manualDiscountOverride", cart.isManualDiscountOverride());
        snapshot.put("taxRate", cart.getTaxRate());
        snapshot.put("tax", cart.getTax());
        snapshot.put("total", cart.getTotal());
        snapshot.put("customerId", cart.getCustomerId());
        List<Map<String, Object>> items = new ArrayList<>();
        for (CartItem item : cart.getItems()) {
            Map<String, Object> value = new LinkedHashMap<>();
            value.put("productId", item.getProductId());
            value.put("name", item.getName());
            value.put("qty", item.getQty());
            value.put("unitSize", item.getUnitSize());
            value.put("unitId", item.getUnitId());
            value.put("unitPrice", item.getUnitPrice());
            value.put("lineTotal", item.getLineTotal());
            value.put("priceTier", item.getPriceTier() == null ? null : item.getPriceTier().name());
            value.put("unitType", item.getUnitType() == null ? null : item.getUnitType().name());
            value.put("variantId", item.getVariantId());
            value.put("sellUnitId", item.getSellUnitId());
            value.put("sellUnitCode", item.getSellUnitCode());
            value.put("conversionToBase", item.getConversionToBase());
            value.put("priceSource", item.getPriceSource());
            value.put("appliedTierMinQty", item.getAppliedTierMinQty());
            value.put("appliedTierGroupCode", item.getAppliedTierGroupCode());
            value.put("note", item.getNote());
            items.add(value);
        }
        snapshot.put("items", items);
        return snapshot;
    }

    /**
     * Executes the saleSnapshot operation.
     *
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @return {@code Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, Object> saleSnapshot(Sale sale) {
        Map<String, Object> snapshot = new LinkedHashMap<>();
        snapshot.put("saleId", sale.getId());
        snapshot.put("createdAt", sale.getCreatedAt());
        snapshot.put("status", sale.getStatus() == null ? null : sale.getStatus().name());
        snapshot.put("cashierUsername", sale.getCashierUsername());
        snapshot.put("subtotal", sale.getSubtotal());
        snapshot.put("discountType", sale.getDiscountType() == null ? null : sale.getDiscountType().name());
        snapshot.put("discountValue", sale.getDiscountValue());
        snapshot.put("discount", sale.getDiscount());
        snapshot.put("tax", sale.getTax());
        snapshot.put("total", sale.getTotal());
        snapshot.put("paymentMethod", sale.getPaymentMethod() == null ? null : sale.getPaymentMethod().name());
        snapshot.put("customerId", sale.getCustomer() == null ? null : sale.getCustomer().getId());
        snapshot.put("shiftId", sale.getShift() == null ? null : sale.getShift().getId());
        snapshot.put("terminalId", sale.getTerminalId());
        snapshot.put("payments", paymentSnapshot(sale));
        return snapshot;
    }

    /**
     * Executes the paymentSnapshot operation.
     *
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @return {@code List<Map<String, Object>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<Map<String, Object>> paymentSnapshot(Sale sale) {
        List<Map<String, Object>> payments = new ArrayList<>();
        if (sale.getPayments() == null) return payments;
        for (SalePayment payment : sale.getPayments()) {
            Map<String, Object> value = new LinkedHashMap<>();
            value.put("method", payment.getMethod() == null ? null : payment.getMethod().name());
            value.put("amount", payment.getAmount());
            value.put("currencyCode", payment.getCurrencyCode());
            value.put("currencyRate", payment.getCurrencyRate());
            value.put("foreignAmount", payment.getForeignAmount());
            payments.add(value);
        }
        return payments;
    }

    /**
     * Executes the sanitizeTerminalId operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String sanitizeTerminalId(String terminalId) {
        if (terminalId == null) return null;
        String trimmed = terminalId.trim();
        if (trimmed.isEmpty()) return null;
        return trimmed.length() <= 128 ? trimmed : trimmed.substring(0, 128);
    }

    /**
     * Executes the requireCheckoutShift operation.
     *
     * @param shift Parameter of type {@code Shift} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String requireCheckoutShift(Shift shift, String terminalId) {
        if (shift == null || shift.getId() == null || shift.getStatus() != ShiftStatus.OPEN) {
            throw new IllegalStateException(msg("pos.error.openShiftBeforeCheckout"));
        }
        String fromShift = sanitizeTerminalId(shift.getTerminalId());
        String fromRequest = sanitizeTerminalId(terminalId);
        String resolved = fromShift != null ? fromShift : fromRequest;
        if (resolved == null) {
            throw new IllegalStateException(msg("pos.error.terminalRequiredCheckout"));
        }
        return resolved;
    }

    /**
     * Executes the lockProduct operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Product lockProduct(Long productId) {
        if (productId == null) {
            throw new IllegalArgumentException(msg("pos.error.productNotFound"));
        }
        return productRepo.findByIdForUpdate(productId)
                .orElseThrow(() -> new IllegalArgumentException(msg("pos.error.productNotFound")));
    }

    /**
     * Executes the msg operation.
     *
     * @param key Parameter of type {@code String} used by this operation.
     * @param args Parameter of type {@code Object...} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String msg(String key, Object... args) {
        return i18nService.msg(key, args);
    }
}
