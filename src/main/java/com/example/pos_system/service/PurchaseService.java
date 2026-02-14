package com.example.pos_system.service;

import com.example.pos_system.entity.GoodsReceipt;
import com.example.pos_system.entity.GoodsReceiptItem;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.PurchaseOrder;
import com.example.pos_system.entity.PurchaseOrderItem;
import com.example.pos_system.entity.PurchaseOrderStatus;
import com.example.pos_system.entity.StockMovementType;
import com.example.pos_system.entity.Supplier;
import com.example.pos_system.repository.AppUserRepo;
import com.example.pos_system.repository.GoodsReceiptRepo;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.PurchaseOrderRepo;
import com.example.pos_system.repository.SupplierRepo;
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

    public PurchaseService(SupplierRepo supplierRepo,
                           PurchaseOrderRepo purchaseOrderRepo,
                           ProductRepo productRepo,
                           GoodsReceiptRepo goodsReceiptRepo,
                           AppUserRepo appUserRepo,
                           StockMovementService stockMovementService,
                           AuditEventService auditEventService) {
        this.supplierRepo = supplierRepo;
        this.purchaseOrderRepo = purchaseOrderRepo;
        this.productRepo = productRepo;
        this.goodsReceiptRepo = goodsReceiptRepo;
        this.appUserRepo = appUserRepo;
        this.stockMovementService = stockMovementService;
        this.auditEventService = auditEventService;
    }

    @Transactional(readOnly = true)
    public List<PurchaseOrder> listPurchaseOrders() {
        requirePurchasesAccess();
        return purchaseOrderRepo.findAllByOrderByCreatedAtDesc();
    }

    @Transactional(readOnly = true)
    public PurchaseOrder getPurchaseOrder(Long id) {
        requirePurchasesAccess();
        if (id == null) return null;
        return purchaseOrderRepo.findDetailedById(id).orElse(null);
    }

    @Transactional(readOnly = true)
    public List<Supplier> listSuppliers() {
        requirePurchasesAccess();
        return supplierRepo.findAll(Sort.by("name").ascending());
    }

    @Transactional(readOnly = true)
    public List<Product> listProducts() {
        requirePurchasesAccess();
        return productRepo.findAll(Sort.by("name").ascending());
    }

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
            item.setOrderedQty(line.orderedQty());
            item.setReceivedQty(0);
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

        Map<Long, Integer> receivedByProduct = new LinkedHashMap<>();
        for (GoodsReceiptLineInput line : safeLines) {
            if (line == null || line.productId() == null) continue;
            if (line.receivedQty() == null || line.receivedQty() <= 0) continue;

            Product product = productRepo.findById(line.productId())
                    .orElseThrow(() -> new IllegalArgumentException("Product not found for receipt line."));

            if (po != null && po.getItems().stream().noneMatch(i -> i.getProduct().getId().equals(product.getId()))) {
                throw new IllegalStateException("Product " + product.getName() + " is not in this PO.");
            }

            GoodsReceiptItem item = new GoodsReceiptItem();
            item.setGoodsReceipt(grn);
            item.setProduct(product);
            item.setReceivedQty(line.receivedQty());
            item.setUnitCost(safeMoney(line.unitCost()));
            grn.getItems().add(item);

            receivedByProduct.merge(product.getId(), line.receivedQty(), Integer::sum);
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
                    item.getReceivedQty(),
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
                Integer receivedDelta = receivedByProduct.get(item.getProduct().getId());
                if (receivedDelta == null || receivedDelta <= 0) continue;
                int current = item.getReceivedQty() == null ? 0 : item.getReceivedQty();
                item.setReceivedQty(current + receivedDelta);
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

    @Transactional(readOnly = true)
    public List<GoodsReceipt> listGoodsReceipts(LocalDate from, LocalDate to, Long supplierId) {
        requirePurchasesAccess();
        return goodsReceiptRepo.findAllByOrderByReceivedAtDesc().stream()
                .filter(grn -> withinRange(grn, from, to))
                .filter(grn -> matchesSupplier(grn, supplierId))
                .toList();
    }

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

    private void updatePoStatusFromReceipts(PurchaseOrder po) {
        int ordered = 0;
        int received = 0;
        for (PurchaseOrderItem item : po.getItems()) {
            ordered += item.getOrderedQty() == null ? 0 : item.getOrderedQty();
            received += item.getReceivedQty() == null ? 0 : item.getReceivedQty();
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

    private boolean withinRange(GoodsReceipt receipt, LocalDate from, LocalDate to) {
        if (receipt == null || receipt.getReceivedAt() == null) return false;
        LocalDate date = receipt.getReceivedAt().toLocalDate();
        if (from != null && date.isBefore(from)) return false;
        if (to != null && date.isAfter(to)) return false;
        return true;
    }

    private boolean matchesSupplier(GoodsReceipt receipt, Long supplierId) {
        if (supplierId == null) return true;
        Long actual = supplierId(receipt);
        return actual != null && actual.equals(supplierId);
    }

    private Long supplierId(GoodsReceipt receipt) {
        if (receipt == null || receipt.getPurchaseOrder() == null || receipt.getPurchaseOrder().getSupplier() == null) {
            return null;
        }
        return receipt.getPurchaseOrder().getSupplier().getId();
    }

    private String supplierName(GoodsReceipt receipt) {
        if (receipt == null || receipt.getPurchaseOrder() == null || receipt.getPurchaseOrder().getSupplier() == null) {
            return "Unlinked";
        }
        String value = receipt.getPurchaseOrder().getSupplier().getName();
        return value == null || value.isBlank() ? "Unnamed supplier" : value;
    }

    private BigDecimal safeMoney(BigDecimal value) {
        if (value == null) return BigDecimal.ZERO.setScale(4, RoundingMode.HALF_UP);
        return value.max(BigDecimal.ZERO).setScale(4, RoundingMode.HALF_UP);
    }

    private BigDecimal safeOptionalMoney(BigDecimal value) {
        if (value == null) return null;
        return value.max(BigDecimal.ZERO).setScale(4, RoundingMode.HALF_UP);
    }

    private String trimTo(String value, int max) {
        if (value == null) return null;
        String trimmed = value.trim();
        if (trimmed.isEmpty()) return null;
        return trimmed.length() <= max ? trimmed : trimmed.substring(0, max);
    }

    private String currentUsername() {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth == null || !auth.isAuthenticated()) return null;
        String name = auth.getName();
        if (name == null || name.isBlank() || "anonymousUser".equalsIgnoreCase(name)) return null;
        return name;
    }

    private Long currentUserId() {
        String username = currentUsername();
        if (username == null) return null;
        return appUserRepo.findByUsername(username).map(u -> u.getId()).orElse(null);
    }

    private void requireManagePurchases() {
        if (!hasAnyAuthority("PERM_PURCHASES_MANAGE", "ROLE_ADMIN", "ROLE_MANAGER")) {
            throw new AccessDeniedException("Purchase management permission required.");
        }
    }

    private void requireReceivingPost() {
        if (!hasAnyAuthority("PERM_RECEIVING_POST", "PERM_PURCHASES_MANAGE", "ROLE_ADMIN", "ROLE_MANAGER")) {
            throw new AccessDeniedException("Receiving post permission required.");
        }
    }

    private void requirePurchasesAccess() {
        if (!hasAnyAuthority("PERM_PURCHASES_MANAGE", "PERM_RECEIVING_POST", "PERM_VIEW_REPORTS", "ROLE_ADMIN", "ROLE_MANAGER")) {
            throw new AccessDeniedException("Purchase access permission required.");
        }
    }

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
            line.put("orderedQty", item.getOrderedQty());
            line.put("receivedQty", item.getReceivedQty());
            line.put("unitCost", item.getUnitCost());
            line.put("tax", item.getTax());
            line.put("discount", item.getDiscount());
            lines.add(line);
        }
        map.put("items", lines);
        return map;
    }

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
            line.put("receivedQty", item.getReceivedQty());
            line.put("unitCost", item.getUnitCost());
            lines.add(line);
        }
        map.put("items", lines);
        return map;
    }

    public record PurchaseOrderLineInput(Long productId,
                                         Integer orderedQty,
                                         BigDecimal unitCost,
                                         BigDecimal tax,
                                         BigDecimal discount) {}

    public record GoodsReceiptLineInput(Long productId,
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

        private ReceivingAccumulator(LocalDate date, Long supplierId, String supplierName) {
            this.date = date;
            this.supplierId = supplierId;
            this.supplierName = supplierName;
        }

        private ReceivingReportRow toRow() {
            return new ReceivingReportRow(date, supplierId, supplierName, receiptCount, totalQty,
                    totalCost.setScale(2, RoundingMode.HALF_UP));
        }
    }
}
