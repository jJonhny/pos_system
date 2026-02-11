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
import java.util.List;

@Service
@Transactional
public class PosService {

    private final ProductRepo productRepo;
    private final SaleRepo saleRepo;
    private final CustomerRepo customerRepo;
    private final DiscountAuditRepo discountAuditRepo;

    public PosService(ProductRepo productRepo, SaleRepo saleRepo, CustomerRepo customerRepo,
                      DiscountAuditRepo discountAuditRepo) {
        this.productRepo = productRepo;
        this.saleRepo = saleRepo;
        this.customerRepo = customerRepo;
        this.discountAuditRepo = discountAuditRepo;
    }

    public Sale checkout(Cart cart, SalePayment payment, String cashierUsername, Customer customer, Shift shift) {
        if (cart.getItems().isEmpty()) throw new IllegalStateException("Cart is empty");

        PaymentMethod method = payment == null ? PaymentMethod.CASH : payment.getMethod();
        Sale sale = new Sale();
        sale.setCreatedAt(LocalDateTime.now());
        sale.setPaymentMethod(method);
        sale.setStatus(SaleStatus.PAID);
        sale.setCashierUsername(cashierUsername);
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

        for (CartItem ci : cart.getItems()) {
            Product p = productRepo.findById(ci.getProductId())
                    .orElseThrow(() -> new IllegalArgumentException("Product not found"));

            if (p.getStockQty() != null && p.getStockQty() < ci.getEffectiveQty()) {
                throw new IllegalStateException("Insufficient stock for " + p.getName());
            }
            if (p.getStockQty() != null) {
                p.setStockQty(p.getStockQty() - ci.getEffectiveQty());
            }

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
        return saved;
    }

    public Sale checkoutSplit(Cart cart, List<SalePayment> payments, String cashierUsername, Customer customer, Shift shift) {
        if (cart.getItems().isEmpty()) throw new IllegalStateException("Cart is empty");
        if (payments == null || payments.isEmpty()) throw new IllegalStateException("No payments provided");

        Sale sale = new Sale();
        sale.setCreatedAt(LocalDateTime.now());
        sale.setPaymentMethod(PaymentMethod.MIXED);
        sale.setStatus(SaleStatus.PAID);
        sale.setCashierUsername(cashierUsername);
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

        for (CartItem ci : cart.getItems()) {
            Product p = productRepo.findById(ci.getProductId())
                    .orElseThrow(() -> new IllegalArgumentException("Product not found"));

            if (p.getStockQty() != null && p.getStockQty() < ci.getEffectiveQty()) {
                throw new IllegalStateException("Insufficient stock for " + p.getName());
            }
            if (p.getStockQty() != null) {
                p.setStockQty(p.getStockQty() - ci.getEffectiveQty());
            }

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
}
