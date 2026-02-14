package com.example.pos_system.service;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.dto.CartItem;
import com.example.pos_system.entity.*;
import com.example.pos_system.repository.CustomerRepo;
import com.example.pos_system.repository.DiscountAuditRepo;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.SaleRepo;
import org.springframework.stereotype.Service;
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

    public PosService(ProductRepo productRepo, SaleRepo saleRepo, CustomerRepo customerRepo,
                      DiscountAuditRepo discountAuditRepo, AuditEventService auditEventService,
                      StockMovementService stockMovementService) {
        this.productRepo = productRepo;
        this.saleRepo = saleRepo;
        this.customerRepo = customerRepo;
        this.discountAuditRepo = discountAuditRepo;
        this.auditEventService = auditEventService;
        this.stockMovementService = stockMovementService;
    }

    public Sale checkout(Cart cart, SalePayment payment, String cashierUsername, Customer customer, Shift shift) {
        return checkout(cart, payment, cashierUsername, customer, shift, null);
    }

    public Sale checkout(Cart cart, SalePayment payment, String cashierUsername, Customer customer, Shift shift, String terminalId) {
        if (cart.getItems().isEmpty()) throw new IllegalStateException("Cart is empty");
        String checkoutTerminalId = requireCheckoutShift(shift, terminalId);

        PaymentMethod method = payment == null ? PaymentMethod.CASH : payment.getMethod();
        Sale sale = new Sale();
        sale.setCreatedAt(LocalDateTime.now());
        sale.setPaymentMethod(method);
        sale.setStatus(SaleStatus.PAID);
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
            Product p = stockMovementService.recordSale(
                    ci.getProductId(),
                    ci.getEffectiveQty(),
                    null,
                    null,
                    "SALE",
                    String.valueOf(sale.getId()),
                    checkoutTerminalId,
                    "POS checkout"
            );

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

    public Sale checkoutSplit(Cart cart, List<SalePayment> payments, String cashierUsername, Customer customer, Shift shift) {
        return checkoutSplit(cart, payments, cashierUsername, customer, shift, null);
    }

    public Sale checkoutSplit(Cart cart, List<SalePayment> payments, String cashierUsername, Customer customer, Shift shift, String terminalId) {
        if (cart.getItems().isEmpty()) throw new IllegalStateException("Cart is empty");
        if (payments == null || payments.isEmpty()) throw new IllegalStateException("No payments provided");
        String checkoutTerminalId = requireCheckoutShift(shift, terminalId);

        Sale sale = new Sale();
        sale.setCreatedAt(LocalDateTime.now());
        sale.setPaymentMethod(PaymentMethod.MIXED);
        sale.setStatus(SaleStatus.PAID);
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
            Product p = stockMovementService.recordSale(
                    ci.getProductId(),
                    ci.getEffectiveQty(),
                    null,
                    null,
                    "SALE",
                    String.valueOf(sale.getId()),
                    checkoutTerminalId,
                    "POS split checkout"
            );

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

    private void applyLoyaltyPoints(Sale sale, Customer customer) {
        if (customer == null) return;
        int points = sale.getTotal() == null ? 0 : sale.getTotal().setScale(0, RoundingMode.FLOOR).intValue();
        if (points < 0) points = 0;
        sale.setPointsEarned(points);
        Integer existing = customer.getPoints() == null ? 0 : customer.getPoints();
        customer.setPoints(existing + points);
        customerRepo.save(customer);
    }

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

    private Map<String, Object> cartSnapshot(Cart cart) {
        Map<String, Object> snapshot = new LinkedHashMap<>();
        snapshot.put("subtotal", cart.getSubtotal());
        snapshot.put("discountType", cart.getDiscountType() == null ? null : cart.getDiscountType().name());
        snapshot.put("discountValue", cart.getDiscountValue());
        snapshot.put("discount", cart.getDiscount());
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
            value.put("unitPrice", item.getUnitPrice());
            value.put("lineTotal", item.getLineTotal());
            value.put("priceTier", item.getPriceTier() == null ? null : item.getPriceTier().name());
            value.put("unitType", item.getUnitType() == null ? null : item.getUnitType().name());
            value.put("note", item.getNote());
            items.add(value);
        }
        snapshot.put("items", items);
        return snapshot;
    }

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

    private String sanitizeTerminalId(String terminalId) {
        if (terminalId == null) return null;
        String trimmed = terminalId.trim();
        if (trimmed.isEmpty()) return null;
        return trimmed.length() <= 128 ? trimmed : trimmed.substring(0, 128);
    }

    private String requireCheckoutShift(Shift shift, String terminalId) {
        if (shift == null || shift.getId() == null || shift.getStatus() != ShiftStatus.OPEN) {
            throw new IllegalStateException("Open a shift before checkout.");
        }
        String fromShift = sanitizeTerminalId(shift.getTerminalId());
        String fromRequest = sanitizeTerminalId(terminalId);
        String resolved = fromShift != null ? fromShift : fromRequest;
        if (resolved == null) {
            throw new IllegalStateException("Terminal ID is required for checkout.");
        }
        return resolved;
    }

    private Product lockProduct(Long productId) {
        if (productId == null) {
            throw new IllegalArgumentException("Product not found");
        }
        return productRepo.findByIdForUpdate(productId)
                .orElseThrow(() -> new IllegalArgumentException("Product not found"));
    }
}
