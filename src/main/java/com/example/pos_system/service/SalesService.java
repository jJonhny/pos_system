package com.example.pos_system.service;

import com.example.pos_system.entity.Customer;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SaleItem;
import com.example.pos_system.entity.SaleStatus;
import com.example.pos_system.entity.UnitType;
import com.example.pos_system.repository.CustomerRepo;
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
public class SalesService {
    private final SaleRepo saleRepo;
    private final ProductRepo productRepo;
    private final CustomerRepo customerRepo;
    private final AuditEventService auditEventService;

    public SalesService(SaleRepo saleRepo, ProductRepo productRepo, CustomerRepo customerRepo,
                        AuditEventService auditEventService) {
        this.saleRepo = saleRepo;
        this.productRepo = productRepo;
        this.customerRepo = customerRepo;
        this.auditEventService = auditEventService;
    }

    @Transactional
    public ReturnOutcome processReturn(Long id, Map<String, String> params) {
        Sale sale = saleRepo.findById(id).orElseThrow();
        if (sale.getStatus() == SaleStatus.VOID) {
            throw new IllegalArgumentException("Cannot return a voided sale.");
        }
        if (sale.getStatus() == SaleStatus.RETURNED) {
            throw new IllegalArgumentException("Sale is already fully returned.");
        }
        Map<String, Object> before = saleSnapshot(sale);
        boolean anyReturn = false;
        BigDecimal refundSubtotal = BigDecimal.ZERO;
        List<Map<String, Object>> returnedItems = new ArrayList<>();

        for (SaleItem item : sale.getItems()) {
            String key = "returnQty_" + item.getId();
            if (!params.containsKey(key)) continue;
            int requested = parseInt(params.get(key));
            int qty = item.getQty() == null ? 0 : item.getQty();
            int returned = item.getReturnedQty() == null ? 0 : item.getReturnedQty();
            int remaining = Math.max(0, qty - returned);
            if (requested <= 0) continue;
            if (requested > remaining) requested = remaining;
            if (requested <= 0) continue;
            anyReturn = true;
            item.setReturnedQty(returned + requested);
            if (item.getUnitPrice() != null) {
                refundSubtotal = refundSubtotal.add(item.getUnitPrice().multiply(BigDecimal.valueOf(requested)));
            }
            Product product = item.getProduct();
            if (product != null && product.getId() != null) {
                Product lockedProduct = productRepo.findByIdForUpdate(product.getId()).orElse(product);
                int unitSize = unitSize(item);
                Integer currentStock = lockedProduct.getStockQty();
                if (currentStock == null) {
                    currentStock = 0;
                }
                lockedProduct.setStockQty(currentStock + (requested * unitSize));
                productRepo.save(lockedProduct);
                product = lockedProduct;
            }
            Map<String, Object> returnedRow = new LinkedHashMap<>();
            returnedRow.put("saleItemId", item.getId());
            returnedRow.put("productId", product == null ? null : product.getId());
            returnedRow.put("qtyReturned", requested);
            returnedRow.put("unitType", item.getUnitType() == null ? null : item.getUnitType().name());
            returnedRow.put("unitSize", unitSize(item));
            returnedItems.add(returnedRow);
        }

        if (!anyReturn) {
            throw new IllegalArgumentException("No return quantities selected.");
        }

        BigDecimal saleSubtotal = safeAmount(sale.getSubtotal());
        BigDecimal saleDiscount = safeAmount(sale.getDiscount());
        BigDecimal saleTax = safeAmount(sale.getTax());

        BigDecimal discountRatio = saleSubtotal.compareTo(BigDecimal.ZERO) == 0
                ? BigDecimal.ZERO
                : saleDiscount.divide(saleSubtotal, 4, RoundingMode.HALF_UP);
        BigDecimal taxableBase = saleSubtotal.subtract(saleDiscount);
        BigDecimal taxRatio = taxableBase.compareTo(BigDecimal.ZERO) == 0
                ? BigDecimal.ZERO
                : saleTax.divide(taxableBase, 4, RoundingMode.HALF_UP);

        BigDecimal refundDiscount = refundSubtotal.multiply(discountRatio);
        BigDecimal refundTaxable = refundSubtotal.subtract(refundDiscount);
        BigDecimal refundTax = refundTaxable.multiply(taxRatio);
        BigDecimal refundTotal = refundSubtotal.subtract(refundDiscount).add(refundTax);

        BigDecimal refunded = safeAmount(sale.getRefundedTotal()).add(refundTotal);
        BigDecimal saleTotal = safeAmount(sale.getTotal());
        if (refunded.compareTo(saleTotal) > 0) refunded = saleTotal;
        sale.setRefundedTotal(refunded);

        boolean fullyReturned = sale.getItems().stream()
                .allMatch(it -> {
                    int qty = it.getQty() == null ? 0 : it.getQty();
                    int returned = it.getReturnedQty() == null ? 0 : it.getReturnedQty();
                    return returned >= qty;
                });
        if (fullyReturned) {
            sale.setStatus(SaleStatus.RETURNED);
        } else {
            sale.setStatus(SaleStatus.PARTIALLY_RETURNED);
        }

        Customer customer = sale.getCustomer();
        if (customer != null) {
            int pointsToDeduct = refundTotal.compareTo(BigDecimal.ZERO) <= 0 ? 0
                    : refundTotal.setScale(0, RoundingMode.FLOOR).intValue();
            int current = customer.getPoints() == null ? 0 : customer.getPoints();
            customer.setPoints(Math.max(0, current - pointsToDeduct));
            Integer earned = sale.getPointsEarned() == null ? 0 : sale.getPointsEarned();
            sale.setPointsEarned(Math.max(0, earned - pointsToDeduct));
            customerRepo.save(customer);
        }

        Sale saved = saleRepo.save(sale);
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("refundTotal", refundTotal);
        metadata.put("returnedItems", returnedItems);
        auditEventService.record("SALE_RETURN", "SALE", saved.getId(), before, saleSnapshot(saved), metadata);
        return new ReturnOutcome(saved.getId(), refundTotal);
    }

    @Transactional
    public VoidOutcome voidSale(Long id) {
        Sale sale = saleRepo.findById(id).orElseThrow();
        if (sale.getStatus() == SaleStatus.VOID) {
            return new VoidOutcome(sale.getId(), false);
        }
        Map<String, Object> before = saleSnapshot(sale);
        sale.setStatus(SaleStatus.VOID);
        Sale saved = saleRepo.save(sale);
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("voidedAt", LocalDateTime.now());
        auditEventService.record("SALE_VOID", "SALE", saved.getId(), before, saleSnapshot(saved), metadata);
        return new VoidOutcome(saved.getId(), true);
    }

    private Map<String, Object> saleSnapshot(Sale sale) {
        Map<String, Object> snapshot = new LinkedHashMap<>();
        snapshot.put("id", sale.getId());
        snapshot.put("status", sale.getStatus() == null ? null : sale.getStatus().name());
        snapshot.put("subtotal", sale.getSubtotal());
        snapshot.put("discount", sale.getDiscount());
        snapshot.put("tax", sale.getTax());
        snapshot.put("total", sale.getTotal());
        snapshot.put("refundedTotal", sale.getRefundedTotal());
        snapshot.put("paymentMethod", sale.getPaymentMethod() == null ? null : sale.getPaymentMethod().name());
        snapshot.put("cashierUsername", sale.getCashierUsername());
        return snapshot;
    }

    private BigDecimal safeAmount(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    private int unitSize(SaleItem item) {
        if (item == null || item.getUnitSize() == null || item.getUnitSize() <= 0) return 1;
        return item.getUnitSize();
    }

    private int parseInt(String value) {
        try {
            return Integer.parseInt(value == null ? "0" : value.trim());
        } catch (NumberFormatException ex) {
            return 0;
        }
    }

    public record ReturnOutcome(Long saleId, BigDecimal refundTotal) {}

    public record VoidOutcome(Long saleId, boolean changed) {}
}
