package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.Customer;
import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.Sale;
import com.devcore.pos_system.entity.SaleItem;
import com.devcore.pos_system.entity.SaleStatus;
import com.devcore.pos_system.entity.UnitType;
import com.devcore.pos_system.repository.CustomerRepo;
import com.devcore.pos_system.repository.SaleRepo;
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
    private final CustomerRepo customerRepo;
    private final AuditEventService auditEventService;
    private final StockMovementService stockMovementService;

    /**
     * Executes the SalesService operation.
     * <p>Return value: A fully initialized SalesService instance.</p>
     *
     * @param saleRepo Parameter of type {@code SaleRepo} used by this operation.
     * @param customerRepo Parameter of type {@code CustomerRepo} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * @param stockMovementService Parameter of type {@code StockMovementService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public SalesService(SaleRepo saleRepo, CustomerRepo customerRepo,
                        AuditEventService auditEventService, StockMovementService stockMovementService) {
        this.saleRepo = saleRepo;
        this.customerRepo = customerRepo;
        this.auditEventService = auditEventService;
        this.stockMovementService = stockMovementService;
    }

    /**
     * Executes the processReturn operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param params Parameter of type {@code Map<String, String>} used by this operation.
     * @return {@code ReturnOutcome} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the processReturn operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param params Parameter of type {@code Map<String, String>} used by this operation.
     * @return {@code ReturnOutcome} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the processReturn operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param params Parameter of type {@code Map<String, String>} used by this operation.
     * @return {@code ReturnOutcome} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
                int requestedBase = toBaseQty(item, requested);
                product = stockMovementService.recordReturn(
                        product.getId(),
                        requestedBase,
                        item.getUnitId(),
                        null,
                        null,
                        "SALE",
                        String.valueOf(sale.getId()),
                        sale.getTerminalId(),
                        "Sale return"
                );
            }
            Map<String, Object> returnedRow = new LinkedHashMap<>();
            returnedRow.put("saleItemId", item.getId());
            returnedRow.put("productId", product == null ? null : product.getId());
            returnedRow.put("qtyReturned", requested);
            returnedRow.put("qtyBaseReturned", toBaseQty(item, requested));
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

    /**
     * Executes the voidSale operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code VoidOutcome} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the voidSale operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code VoidOutcome} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the voidSale operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code VoidOutcome} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional
    public VoidOutcome voidSale(Long id) {
        Sale sale = saleRepo.findById(id).orElseThrow();
        if (sale.getStatus() == SaleStatus.VOID) {
            return new VoidOutcome(sale.getId(), false);
        }
        Map<String, Object> before = saleSnapshot(sale);
        List<Map<String, Object>> reversed = new ArrayList<>();
        for (SaleItem item : sale.getItems()) {
            Product product = item.getProduct();
            if (product == null || product.getId() == null) continue;
            int soldQtyBase = lineQtyBase(item);
            int returnedQtyBase = toBaseQty(item, item.getReturnedQty() == null ? 0 : item.getReturnedQty());
            int toReverse = soldQtyBase - returnedQtyBase;
            if (toReverse <= 0) continue;
            stockMovementService.recordVoid(
                    product.getId(),
                    toReverse,
                    item.getUnitId(),
                    null,
                    null,
                    "SALE",
                    String.valueOf(sale.getId()),
                    sale.getTerminalId(),
                    "Void sale compensation"
            );
            Map<String, Object> row = new LinkedHashMap<>();
            row.put("saleItemId", item.getId());
            row.put("productId", product.getId());
            row.put("qtyReversed", toReverse);
            reversed.add(row);
        }
        sale.setStatus(SaleStatus.VOID);
        Sale saved = saleRepo.save(sale);
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("voidedAt", LocalDateTime.now());
        metadata.put("reversedItems", reversed);
        auditEventService.record("SALE_VOID", "SALE", saved.getId(), before, saleSnapshot(saved), metadata);
        return new VoidOutcome(saved.getId(), true);
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

    /**
     * Executes the safeAmount operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeAmount(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    /**
     * Executes the unitSize operation.
     *
     * @param item Parameter of type {@code SaleItem} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int unitSize(SaleItem item) {
        if (item == null || item.getUnitSize() == null || item.getUnitSize() <= 0) return 1;
        return item.getUnitSize();
    }

    private int lineQtyBase(SaleItem item) {
        if (item == null) return 0;
        if (item.getQtyBase() != null && item.getQtyBase() > 0) return item.getQtyBase();
        int qty = item.getQty() == null ? 0 : item.getQty();
        return qty * unitSize(item);
    }

    private int toBaseQty(SaleItem item, int unitQty) {
        int safeUnitQty = Math.max(0, unitQty);
        if (safeUnitQty == 0 || item == null) return 0;
        int totalUnits = item.getQty() == null ? 0 : Math.max(0, item.getQty());
        int totalBase = lineQtyBase(item);
        if (totalUnits <= 0 || totalBase <= 0) {
            return safeUnitQty * unitSize(item);
        }
        BigDecimal basePerUnit = BigDecimal.valueOf(totalBase)
                .divide(BigDecimal.valueOf(totalUnits), 6, RoundingMode.HALF_UP);
        return basePerUnit.multiply(BigDecimal.valueOf(safeUnitQty))
                .setScale(0, RoundingMode.HALF_UP)
                .intValue();
    }

    /**
     * Executes the parseInt operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
