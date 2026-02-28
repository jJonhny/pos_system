package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.GoodsReceipt;
import com.devcore.pos_system.entity.GoodsReceiptItem;
import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.PurchaseOrder;
import com.devcore.pos_system.entity.PurchaseOrderItem;
import com.devcore.pos_system.entity.PurchaseOrderStatus;
import com.devcore.pos_system.entity.StockMovementType;
import com.devcore.pos_system.entity.Supplier;
import com.devcore.pos_system.repository.AppUserRepo;
import com.devcore.pos_system.repository.GoodsReceiptRepo;
import com.devcore.pos_system.repository.ProductRepo;
import com.devcore.pos_system.repository.PurchaseOrderRepo;
import com.devcore.pos_system.repository.SupplierRepo;
import org.springframework.data.domain.Sort;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

@Service
@Transactional
public class PurchaseService {
    private final SupplierRepo supplierRepo;
    private final PurchaseOrderRepo purchaseOrderRepo;
    private final ProductRepo productRepo;
    private final GoodsReceiptRepo goodsReceiptRepo;
    private final AppUserRepo appUserRepo;
    private final StockMovementService stockMovementService;
    private final AuditEventService auditEventService;
    private final ProductUnitConversionService productUnitConversionService;

    /**
     * Executes the PurchaseService operation.
     * <p>Return value: A fully initialized PurchaseService instance.</p>
     *
     * @param supplierRepo Parameter of type {@code SupplierRepo} used by this operation.
     * @param purchaseOrderRepo Parameter of type {@code PurchaseOrderRepo} used by this operation.
     * @param productRepo Parameter of type {@code ProductRepo} used by this operation.
     * @param goodsReceiptRepo Parameter of type {@code GoodsReceiptRepo} used by this operation.
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * @param stockMovementService Parameter of type {@code StockMovementService} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public PurchaseService(SupplierRepo supplierRepo,
                           PurchaseOrderRepo purchaseOrderRepo,
                           ProductRepo productRepo,
                           GoodsReceiptRepo goodsReceiptRepo,
                           AppUserRepo appUserRepo,
                           StockMovementService stockMovementService,
                           AuditEventService auditEventService,
                           ProductUnitConversionService productUnitConversionService) {
        this.supplierRepo = supplierRepo;
        this.purchaseOrderRepo = purchaseOrderRepo;
        this.productRepo = productRepo;
        this.goodsReceiptRepo = goodsReceiptRepo;
        this.appUserRepo = appUserRepo;
        this.stockMovementService = stockMovementService;
        this.auditEventService = auditEventService;
        this.productUnitConversionService = productUnitConversionService;
    }

    /**
     * Executes the listPurchaseOrders operation.
     *
     * @return {@code List<PurchaseOrder>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the listPurchaseOrders operation.
     *
     * @return {@code List<PurchaseOrder>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the listPurchaseOrders operation.
     *
     * @return {@code List<PurchaseOrder>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public List<PurchaseOrder> listPurchaseOrders() {
        requirePurchasesAccess();
        return purchaseOrderRepo.findAllByOrderByCreatedAtDesc();
    }

    /**
     * Executes the getPurchaseOrder operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code PurchaseOrder} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the getPurchaseOrder operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code PurchaseOrder} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the getPurchaseOrder operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code PurchaseOrder} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public PurchaseOrder getPurchaseOrder(Long id) {
        requirePurchasesAccess();
        if (id == null) return null;
        return purchaseOrderRepo.findDetailedById(id).orElse(null);
    }

    /**
     * Executes the listSuppliers operation.
     *
     * @return {@code List<Supplier>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the listSuppliers operation.
     *
     * @return {@code List<Supplier>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the listSuppliers operation.
     *
     * @return {@code List<Supplier>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public List<Supplier> listSuppliers() {
        requirePurchasesAccess();
        return supplierRepo.findAll(Sort.by("name").ascending());
    }

    /**
     * Executes the listProducts operation.
     *
     * @return {@code List<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the listProducts operation.
     *
     * @return {@code List<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the listProducts operation.
     *
     * @return {@code List<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public List<Product> listProducts() {
        requirePurchasesAccess();
        return productRepo.findAll(Sort.by("name").ascending());
    }

    /**
     * Executes the savePurchaseOrder operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @param status Parameter of type {@code PurchaseOrderStatus} used by this operation.
     * @param currency Parameter of type {@code String} used by this operation.
     * @param expectedAt Parameter of type {@code LocalDate} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @param lines Parameter of type {@code List<PurchaseOrderLineInput>} used by this operation.
     * @return {@code PurchaseOrder} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public PurchaseOrder savePurchaseOrder(Long id,
                                           Long supplierId,
                                           PurchaseOrderStatus status,
                                           String currency,
                                           LocalDate expectedAt,
                                           String notes,
                                           List<PurchaseOrderLineInput> lines) {
        requireManagePurchases();
        Supplier supplier = supplierRepo.findById(supplierId)
                .orElseThrow(() -> new IllegalArgumentException("Supplier is required."));
        if (status == PurchaseOrderStatus.PARTIAL || status == PurchaseOrderStatus.RECEIVED) {
            throw new IllegalArgumentException("Status PARTIAL/RECEIVED is managed automatically by receipts.");
        }

        PurchaseOrder po = id == null
                /**
                 * Executes the PurchaseOrder operation.
                 *
                 * @return {@code ? new} Result produced by this operation.
                 * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
                 * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
                 */
                ? new PurchaseOrder()
                : purchaseOrderRepo.findDetailedById(id)
                .orElseThrow(() -> new IllegalArgumentException("Purchase order not found."));

        if (po.getId() != null && (po.getStatus() == PurchaseOrderStatus.PARTIAL
                || po.getStatus() == PurchaseOrderStatus.RECEIVED
                || po.getStatus() == PurchaseOrderStatus.CANCELED)) {
            throw new IllegalStateException("Cannot edit a " + po.getStatus().name().toLowerCase(Locale.ROOT) + " purchase order.");
        }

        Map<String, Object> before = po.getId() == null ? null : poSnapshot(po);
        po.setSupplier(supplier);
        po.setExpectedAt(expectedAt);
        po.setNotes(trimTo(notes, 1000));
        po.setCurrency(trimTo(currency, 8));

        if (po.getId() == null) {
            po.setCreatedBy(currentUsername());
            po.setCreatedByUserId(currentUserId());
            po.setStatus(status == null ? PurchaseOrderStatus.DRAFT : status);
        } else if (status != null) {
            po.setStatus(status);
        }

        po.getItems().clear();
        List<PurchaseOrderLineInput> safeLines = lines == null ? List.of() : lines;
        for (PurchaseOrderLineInput line : safeLines) {
            if (line == null || line.productId() == null) continue;
            if (line.orderedQty() == null || line.orderedQty() <= 0) continue;

            Product product = productRepo.findById(line.productId())
                    .orElseThrow(() -> new IllegalArgumentException("Product not found for PO line."));

            PurchaseOrderItem item = new PurchaseOrderItem();
            item.setPurchaseOrder(po);
            item.setProduct(product);
            item.setUnitId(line.unitId());
            item.setOrderedQty(line.orderedQty());
            item.setOrderedQtyBase(toBaseQty(product, line.unitId(), line.orderedQty()));
            item.setReceivedQty(0);
            item.setReceivedQtyBase(0);
            item.setUnitCost(safeMoney(line.unitCost()));
            item.setTax(safeOptionalMoney(line.tax()));
            item.setDiscount(safeOptionalMoney(line.discount()));
            po.getItems().add(item);
        }

        if (po.getItems().isEmpty()) {
            throw new IllegalArgumentException("At least one PO line is required.");
        }

        PurchaseOrder saved = purchaseOrderRepo.save(po);
        String action = saved.getId().equals(id) ? "PURCHASE_ORDER_UPDATE" : "PURCHASE_ORDER_CREATE";
        auditEventService.record(action, "PO", saved.getId(), before, poSnapshot(saved), null);
        return saved;
    }

    /**
     * Executes the postGoodsReceipt operation.
     *
     * @param poId Parameter of type {@code Long} used by this operation.
     * @param invoiceNo Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param lines Parameter of type {@code List<GoodsReceiptLineInput>} used by this operation.
     * @return {@code GoodsReceipt} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public GoodsReceipt postGoodsReceipt(Long poId,
                                         String invoiceNo,
                                         String notes,
                                         String terminalId,
                                         List<GoodsReceiptLineInput> lines) {
        requireReceivingPost();
        List<GoodsReceiptLineInput> safeLines = lines == null ? List.of() : lines;
        if (safeLines.isEmpty()) {
            throw new IllegalArgumentException("At least one received line is required.");
        }

        PurchaseOrder po = null;
        if (poId != null) {
            po = purchaseOrderRepo.findDetailedById(poId)
                    .orElseThrow(() -> new IllegalArgumentException("Purchase order not found."));
            if (po.getStatus() == PurchaseOrderStatus.CANCELED) {
                throw new IllegalStateException("Cannot receive against a canceled PO.");
            }
        }

        GoodsReceipt grn = new GoodsReceipt();
        grn.setPurchaseOrder(po);
        grn.setInvoiceNo(trimTo(invoiceNo, 120));
        grn.setNotes(trimTo(notes, 1000));
        grn.setReceivedBy(currentUsername());
        grn.setReceivedByUserId(currentUserId());

        Map<Long, Integer> receivedBaseByProduct = new LinkedHashMap<>();
        for (GoodsReceiptLineInput line : safeLines) {
            if (line == null || line.productId() == null) continue;
            if (line.receivedQty() == null || line.receivedQty() <= 0) continue;

            Product product = productRepo.findById(line.productId())
                    .orElseThrow(() -> new IllegalArgumentException("Product not found for receipt line."));

            if (po != null && po.getItems().stream().noneMatch(i -> i.getProduct() != null
                    && i.getProduct().getId() != null
                    && i.getProduct().getId().equals(product.getId()))) {
                throw new IllegalStateException("Product " + product.getName() + " is not in this PO.");
            }

            GoodsReceiptItem item = new GoodsReceiptItem();
            item.setGoodsReceipt(grn);
            item.setProduct(product);
            item.setUnitId(line.unitId());
            item.setReceivedQty(line.receivedQty());
            int receivedBase = toBaseQty(product, line.unitId(), line.receivedQty());
            item.setReceivedQtyBase(receivedBase);
            item.setUnitCost(safeMoney(line.unitCost()));
            grn.getItems().add(item);

            receivedBaseByProduct.merge(product.getId(), receivedBase, Integer::sum);
        }

        if (grn.getItems().isEmpty()) {
            throw new IllegalArgumentException("At least one valid receipt line is required.");
        }

        GoodsReceipt saved = goodsReceiptRepo.save(grn);

        String currency = po == null ? null : po.getCurrency();
        String refId = String.valueOf(saved.getId());
        for (GoodsReceiptItem item : saved.getItems()) {
            stockMovementService.recordReceive(
                    item.getProduct().getId(),
                    safeInt(item.getReceivedQtyBase()),
                    item.getUnitId(),
                    item.getUnitCost(),
                    currency,
                    "GRN",
                    refId,
                    terminalId,
                    "Supplier receiving"
            );
        }

        if (po != null) {
            for (PurchaseOrderItem item : po.getItems()) {
                if (item.getProduct() == null || item.getProduct().getId() == null) continue;
                Integer receivedDeltaBase = receivedBaseByProduct.get(item.getProduct().getId());
                if (receivedDeltaBase == null || receivedDeltaBase <= 0) continue;
                int currentBase = safeInt(item.getReceivedQtyBase());
                int nextBase = currentBase + receivedDeltaBase;
                item.setReceivedQtyBase(nextBase);
                item.setReceivedQty(deriveDisplayQty(nextBase, item.getOrderedQty(), item.getOrderedQtyBase()));
            }
            updatePoStatusFromReceipts(po);
            purchaseOrderRepo.save(po);
        }

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("poId", po == null ? null : po.getId());
        metadata.put("lineCount", saved.getItems().size());
        metadata.put("terminalId", trimTo(terminalId, 128));
        auditEventService.record("GOODS_RECEIPT_POST", "GRN", saved.getId(), null, grnSnapshot(saved), metadata);
        return saved;
    }

    /**
     * Executes the listGoodsReceipts operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @return {@code List<GoodsReceipt>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the listGoodsReceipts operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @return {@code List<GoodsReceipt>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the listGoodsReceipts operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @return {@code List<GoodsReceipt>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public List<GoodsReceipt> listGoodsReceipts(LocalDate from, LocalDate to, Long supplierId) {
        requirePurchasesAccess();
        return goodsReceiptRepo.findAllByOrderByReceivedAtDesc().stream()
                .filter(grn -> withinRange(grn, from, to))
                .filter(grn -> matchesSupplier(grn, supplierId))
                .toList();
    }

    /**
     * Executes the buildReceivingReport operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @return {@code List<ReceivingReportRow>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the buildReceivingReport operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @return {@code List<ReceivingReportRow>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the buildReceivingReport operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @return {@code List<ReceivingReportRow>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public List<ReceivingReportRow> buildReceivingReport(LocalDate from, LocalDate to, Long supplierId) {
        List<GoodsReceipt> receipts = listGoodsReceipts(from, to, supplierId);
        Map<String, ReceivingAccumulator> grouped = new LinkedHashMap<>();
        for (GoodsReceipt receipt : receipts) {
            LocalDate date = receipt.getReceivedAt() == null ? null : receipt.getReceivedAt().toLocalDate();
            String supplierName = supplierName(receipt);
            Long sid = supplierId(receipt);
            String key = (date == null ? "" : date) + "|" + (sid == null ? "0" : sid);
            ReceivingAccumulator acc = grouped.computeIfAbsent(key,
                    ignored -> new ReceivingAccumulator(date, sid, supplierName));
            acc.receiptCount++;
            for (GoodsReceiptItem item : receipt.getItems()) {
                int qty = item.getReceivedQty() == null ? 0 : item.getReceivedQty();
                acc.totalQty += Math.max(qty, 0);
                BigDecimal cost = safeMoney(item.getUnitCost()).multiply(BigDecimal.valueOf(Math.max(qty, 0)));
                acc.totalCost = acc.totalCost.add(cost);
            }
        }

        return grouped.values().stream()
                .map(ReceivingAccumulator::toRow)
                .sorted(Comparator.comparing(ReceivingReportRow::date, Comparator.nullsLast(Comparator.reverseOrder()))
                        .thenComparing(ReceivingReportRow::supplierName, Comparator.nullsLast(String::compareToIgnoreCase)))
                .toList();
    }

    /**
     * Executes the updatePoStatusFromReceipts operation.
     *
     * @param po Parameter of type {@code PurchaseOrder} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void updatePoStatusFromReceipts(PurchaseOrder po) {
        int ordered = 0;
        int received = 0;
        for (PurchaseOrderItem item : po.getItems()) {
            int orderedBase = item.getOrderedQtyBase() == null
                    ? safeInt(item.getOrderedQty())
                    : safeInt(item.getOrderedQtyBase());
            int receivedBase = item.getReceivedQtyBase() == null
                    ? safeInt(item.getReceivedQty())
                    : safeInt(item.getReceivedQtyBase());
            ordered += orderedBase;
            received += receivedBase;
        }

        if (ordered <= 0) {
            po.setStatus(PurchaseOrderStatus.DRAFT);
            return;
        }

        if (received <= 0) {
            if (po.getStatus() == PurchaseOrderStatus.DRAFT || po.getStatus() == PurchaseOrderStatus.SENT) {
                return;
            }
            po.setStatus(PurchaseOrderStatus.SENT);
            return;
        }

        if (received < ordered) {
            po.setStatus(PurchaseOrderStatus.PARTIAL);
        } else {
            po.setStatus(PurchaseOrderStatus.RECEIVED);
        }
    }

    /**
     * Executes the withinRange operation.
     *
     * @param receipt Parameter of type {@code GoodsReceipt} used by this operation.
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean withinRange(GoodsReceipt receipt, LocalDate from, LocalDate to) {
        if (receipt == null || receipt.getReceivedAt() == null) return false;
        LocalDate date = receipt.getReceivedAt().toLocalDate();
        if (from != null && date.isBefore(from)) return false;
        if (to != null && date.isAfter(to)) return false;
        return true;
    }

    /**
     * Executes the matchesSupplier operation.
     *
     * @param receipt Parameter of type {@code GoodsReceipt} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean matchesSupplier(GoodsReceipt receipt, Long supplierId) {
        if (supplierId == null) return true;
        Long actual = supplierId(receipt);
        return actual != null && actual.equals(supplierId);
    }

    /**
     * Executes the supplierId operation.
     *
     * @param receipt Parameter of type {@code GoodsReceipt} used by this operation.
     * @return {@code Long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Long supplierId(GoodsReceipt receipt) {
        if (receipt == null || receipt.getPurchaseOrder() == null || receipt.getPurchaseOrder().getSupplier() == null) {
            return null;
        }
        return receipt.getPurchaseOrder().getSupplier().getId();
    }

    /**
     * Executes the supplierName operation.
     *
     * @param receipt Parameter of type {@code GoodsReceipt} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String supplierName(GoodsReceipt receipt) {
        if (receipt == null || receipt.getPurchaseOrder() == null || receipt.getPurchaseOrder().getSupplier() == null) {
            return "Unlinked";
        }
        String value = receipt.getPurchaseOrder().getSupplier().getName();
        return value == null || value.isBlank() ? "Unnamed supplier" : value;
    }

    private int toBaseQty(Product product, Long unitId, Integer qty) {
        int safeQty = safeInt(qty);
        if (safeQty <= 0) return 0;
        return productUnitConversionService.toBaseInt(product, unitId, BigDecimal.valueOf(safeQty));
    }

    private int deriveDisplayQty(int qtyBase, Integer orderedQty, Integer orderedQtyBase) {
        int safeBase = Math.max(0, qtyBase);
        int safeOrdered = safeInt(orderedQty);
        int safeOrderedBase = safeInt(orderedQtyBase);
        if (safeOrdered <= 0 || safeOrderedBase <= 0) {
            return safeBase;
        }
        BigDecimal ratio = BigDecimal.valueOf(safeOrdered)
                .divide(BigDecimal.valueOf(safeOrderedBase), 6, RoundingMode.HALF_UP);
        return ratio.multiply(BigDecimal.valueOf(safeBase))
                .setScale(0, RoundingMode.HALF_UP)
                .intValue();
    }

    private int safeInt(Integer value) {
        return value == null ? 0 : Math.max(0, value);
    }

    /**
     * Executes the safeMoney operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeMoney(BigDecimal value) {
        if (value == null) return BigDecimal.ZERO.setScale(4, RoundingMode.HALF_UP);
        return value.max(BigDecimal.ZERO).setScale(4, RoundingMode.HALF_UP);
    }

    /**
     * Executes the safeOptionalMoney operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeOptionalMoney(BigDecimal value) {
        if (value == null) return null;
        return value.max(BigDecimal.ZERO).setScale(4, RoundingMode.HALF_UP);
    }

    /**
     * Executes the trimTo operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @param max Parameter of type {@code int} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String trimTo(String value, int max) {
        if (value == null) return null;
        String trimmed = value.trim();
        if (trimmed.isEmpty()) return null;
        return trimmed.length() <= max ? trimmed : trimmed.substring(0, max);
    }

    /**
     * Executes the currentUsername operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String currentUsername() {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth == null || !auth.isAuthenticated()) return null;
        String name = auth.getName();
        if (name == null || name.isBlank() || "anonymousUser".equalsIgnoreCase(name)) return null;
        return name;
    }

    /**
     * Executes the currentUserId operation.
     *
     * @return {@code Long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Long currentUserId() {
        String username = currentUsername();
        if (username == null) return null;
        return appUserRepo.findByUsername(username).map(u -> u.getId()).orElse(null);
    }

    /**
     * Executes the requireManagePurchases operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void requireManagePurchases() {
        if (!hasAnyAuthority("PERM_PURCHASES_MANAGE", "ROLE_ADMIN", "ROLE_MANAGER")) {
            throw new AccessDeniedException("Purchase management permission required.");
        }
    }

    /**
     * Executes the requireReceivingPost operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void requireReceivingPost() {
        if (!hasAnyAuthority("PERM_RECEIVING_POST", "PERM_PURCHASES_MANAGE", "ROLE_ADMIN", "ROLE_MANAGER")) {
            throw new AccessDeniedException("Receiving post permission required.");
        }
    }

    /**
     * Executes the requirePurchasesAccess operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void requirePurchasesAccess() {
        if (!hasAnyAuthority("PERM_PURCHASES_MANAGE", "PERM_RECEIVING_POST", "PERM_VIEW_REPORTS", "ROLE_ADMIN", "ROLE_MANAGER")) {
            throw new AccessDeniedException("Purchase access permission required.");
        }
    }

    /**
     * Executes the hasAnyAuthority operation.
     *
     * @param required Parameter of type {@code String...} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean hasAnyAuthority(String... required) {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth == null || !auth.isAuthenticated()) return false;
        for (var authority : auth.getAuthorities()) {
            String value = authority.getAuthority();
            if (value == null) continue;
            for (String expected : required) {
                if (expected.equalsIgnoreCase(value)) return true;
            }
        }
        return false;
    }

    /**
     * Executes the poSnapshot operation.
     *
     * @param po Parameter of type {@code PurchaseOrder} used by this operation.
     * @return {@code Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, Object> poSnapshot(PurchaseOrder po) {
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("id", po.getId());
        map.put("status", po.getStatus() == null ? null : po.getStatus().name());
        map.put("supplierId", po.getSupplier() == null ? null : po.getSupplier().getId());
        map.put("currency", po.getCurrency());
        map.put("expectedAt", po.getExpectedAt());
        map.put("notes", po.getNotes());
        List<Map<String, Object>> lines = new ArrayList<>();
        for (PurchaseOrderItem item : po.getItems()) {
            Map<String, Object> line = new LinkedHashMap<>();
            line.put("productId", item.getProduct() == null ? null : item.getProduct().getId());
            line.put("unitId", item.getUnitId());
            line.put("orderedQty", item.getOrderedQty());
            line.put("orderedQtyBase", item.getOrderedQtyBase());
            line.put("receivedQty", item.getReceivedQty());
            line.put("receivedQtyBase", item.getReceivedQtyBase());
            line.put("unitCost", item.getUnitCost());
            line.put("tax", item.getTax());
            line.put("discount", item.getDiscount());
            lines.add(line);
        }
        map.put("items", lines);
        return map;
    }

    /**
     * Executes the grnSnapshot operation.
     *
     * @param grn Parameter of type {@code GoodsReceipt} used by this operation.
     * @return {@code Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, Object> grnSnapshot(GoodsReceipt grn) {
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("id", grn.getId());
        map.put("poId", grn.getPurchaseOrder() == null ? null : grn.getPurchaseOrder().getId());
        map.put("receivedAt", grn.getReceivedAt());
        map.put("receivedBy", grn.getReceivedBy());
        map.put("invoiceNo", grn.getInvoiceNo());
        map.put("notes", grn.getNotes());
        List<Map<String, Object>> lines = new ArrayList<>();
        for (GoodsReceiptItem item : grn.getItems()) {
            Map<String, Object> line = new LinkedHashMap<>();
            line.put("productId", item.getProduct() == null ? null : item.getProduct().getId());
            line.put("unitId", item.getUnitId());
            line.put("receivedQty", item.getReceivedQty());
            line.put("receivedQtyBase", item.getReceivedQtyBase());
            line.put("unitCost", item.getUnitCost());
            lines.add(line);
        }
        map.put("items", lines);
        return map;
    }

    public record PurchaseOrderLineInput(Long productId,
                                         Long unitId,
                                         Integer orderedQty,
                                         BigDecimal unitCost,
                                         BigDecimal tax,
                                         BigDecimal discount) {}

    public record GoodsReceiptLineInput(Long productId,
                                        Long unitId,
                                        Integer receivedQty,
                                        BigDecimal unitCost) {}

    public record ReceivingReportRow(LocalDate date,
                                     Long supplierId,
                                     String supplierName,
                                     long receiptCount,
                                     int totalQty,
                                     BigDecimal totalCost) {}

    private static class ReceivingAccumulator {
        private final LocalDate date;
        private final Long supplierId;
        private final String supplierName;
        private long receiptCount;
        private int totalQty;
        private BigDecimal totalCost = BigDecimal.ZERO;

        /**
         * Executes the ReceivingAccumulator operation.
         * <p>Return value: A fully initialized ReceivingAccumulator instance.</p>
         *
         * @param date Parameter of type {@code LocalDate} used by this operation.
         * @param supplierId Parameter of type {@code Long} used by this operation.
         * @param supplierName Parameter of type {@code String} used by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private ReceivingAccumulator(LocalDate date, Long supplierId, String supplierName) {
            this.date = date;
            this.supplierId = supplierId;
            this.supplierName = supplierName;
        }

        /**
         * Executes the toRow operation.
         *
         * @return {@code ReceivingReportRow} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private ReceivingReportRow toRow() {
            return new ReceivingReportRow(date, supplierId, supplierName, receiptCount, totalQty,
                    totalCost.setScale(2, RoundingMode.HALF_UP));
        }
    }
}
