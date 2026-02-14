package com.example.pos_system.config;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.entity.Category;
import com.example.pos_system.entity.GoodsReceipt;
import com.example.pos_system.entity.GoodsReceiptItem;
import com.example.pos_system.entity.PaymentMethod;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.PurchaseOrder;
import com.example.pos_system.entity.PurchaseOrderItem;
import com.example.pos_system.entity.PurchaseOrderStatus;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SalePayment;
import com.example.pos_system.entity.Shift;
import com.example.pos_system.entity.ShiftStatus;
import com.example.pos_system.entity.Supplier;
import com.example.pos_system.entity.SupplierStatus;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.GoodsReceiptRepo;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.PurchaseOrderRepo;
import com.example.pos_system.repository.SaleRepo;
import com.example.pos_system.repository.SupplierRepo;
import com.example.pos_system.service.InventoryService;
import com.example.pos_system.service.PosService;
import com.example.pos_system.service.ShiftService;
import com.example.pos_system.service.StockMovementService;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Component
@Profile("dev")
public class DevDataSeeder implements CommandLineRunner {
    private final CategoryRepo categoryRepo;
    private final ProductRepo productRepo;
    private final SaleRepo saleRepo;
    private final SupplierRepo supplierRepo;
    private final PurchaseOrderRepo purchaseOrderRepo;
    private final GoodsReceiptRepo goodsReceiptRepo;
    private final InventoryService inventoryService;
    private final PosService posService;
    private final ShiftService shiftService;
    private final StockMovementService stockMovementService;

    public DevDataSeeder(CategoryRepo categoryRepo,
                         ProductRepo productRepo,
                         SaleRepo saleRepo,
                         SupplierRepo supplierRepo,
                         PurchaseOrderRepo purchaseOrderRepo,
                         GoodsReceiptRepo goodsReceiptRepo,
                         InventoryService inventoryService,
                         PosService posService,
                         ShiftService shiftService,
                         StockMovementService stockMovementService) {
        this.categoryRepo = categoryRepo;
        this.productRepo = productRepo;
        this.saleRepo = saleRepo;
        this.supplierRepo = supplierRepo;
        this.purchaseOrderRepo = purchaseOrderRepo;
        this.goodsReceiptRepo = goodsReceiptRepo;
        this.inventoryService = inventoryService;
        this.posService = posService;
        this.shiftService = shiftService;
        this.stockMovementService = stockMovementService;
    }

    @Override
    public void run(String... args) {
        seedCategoriesAndProducts();
        seedSampleSales();
        seedSuppliersAndReceiving();
    }

    private void seedCategoriesAndProducts() {
        List<Category> existingCategories = categoryRepo.findAll();
        Map<String, Category> byName = new HashMap<>();
        for (Category c : existingCategories) {
            if (c.getName() != null) byName.put(c.getName().toLowerCase(), c);
        }

        List<Category> toCreate = new ArrayList<>();
        Category beverages = ensureCategory("Beverages", byName, toCreate);
        Category snacks = ensureCategory("Snacks", byName, toCreate);
        Category essentials = ensureCategory("Essentials", byName, toCreate);

        if (!toCreate.isEmpty()) {
            categoryRepo.saveAll(toCreate);
        }

        if (productRepo.count() > 0) return;

        List<ProductSeed> seeds = List.of(
                new ProductSeed("BEV-001", "100000000001", "Cola 330ml", "1.50", 120, beverages,
                        "https://images.unsplash.com/photo-1629203851122-3726ecdf080e?auto=format&fit=crop&w=200&q=80"),
                new ProductSeed("BEV-002", "100000000002", "Orange Juice 1L", "3.20", 60, beverages,
                        "https://images.unsplash.com/photo-1542444459-db0f86b5a7b2?auto=format&fit=crop&w=200&q=80"),
                new ProductSeed("SNK-001", "100000000101", "Potato Chips", "2.10", 80, snacks,
                        "https://images.unsplash.com/photo-1585238342028-4bbc3b8b2d8b?auto=format&fit=crop&w=200&q=80"),
                new ProductSeed("SNK-002", "100000000102", "Chocolate Bar", "1.25", 150, snacks,
                        "https://images.unsplash.com/photo-1541783245831-57d6fb0926d3?auto=format&fit=crop&w=200&q=80"),
                new ProductSeed("ESS-001", "100000000201", "Hand Soap", "2.75", 40, essentials,
                        "https://images.unsplash.com/photo-1583947215259-38e31be8751f?auto=format&fit=crop&w=200&q=80"),
                new ProductSeed("ESS-002", "100000000202", "Paper Towels", "4.50", 30, essentials,
                        "https://images.unsplash.com/photo-1614302264631-ef85d1793b79?auto=format&fit=crop&w=200&q=80")
        );

        for (ProductSeed seed : seeds) {
            Product saved = productRepo.save(newProduct(seed));
            inventoryService.setStockFromImport(
                    saved.getId(),
                    seed.stockQty(),
                    "DEV-SEED-IMPORT:" + seed.sku(),
                    "Dev seed opening stock"
            );
        }
    }

    private void seedSampleSales() {
        if (saleRepo.count() > 0) return;

        List<Product> products = productRepo.findAll();
        if (products.size() < 2) return;

        Shift shift = shiftService.findOpenShift("seed-cashier", "DEV-TERM-01")
                .orElseGet(() -> shiftService.openShift(
                        "seed-cashier",
                        "DEV-TERM-01",
                        Map.of("USD", new BigDecimal("200.00"))
                ));

        Cart firstCart = new Cart();
        firstCart.add(products.get(0));
        firstCart.add(products.get(1));
        firstCart.setTaxRate(new BigDecimal("0.07"));

        SalePayment firstPayment = new SalePayment();
        firstPayment.setMethod(PaymentMethod.CASH);
        firstPayment.setAmount(firstCart.getTotal().setScale(2, RoundingMode.HALF_UP));
        posService.checkout(firstCart, firstPayment, "seed-cashier", null, shift, "DEV-TERM-01");

        if (products.size() > 2) {
            Cart secondCart = new Cart();
            secondCart.add(products.get(2));
            secondCart.add(products.get(2));
            secondCart.setDiscountValue(new BigDecimal("5"));
            secondCart.setDiscountType(com.example.pos_system.entity.DiscountType.PERCENT);

            SalePayment secondPayment = new SalePayment();
            secondPayment.setMethod(PaymentMethod.CARD);
            secondPayment.setAmount(secondCart.getTotal().setScale(2, RoundingMode.HALF_UP));
            posService.checkout(secondCart, secondPayment, "seed-cashier", null, shift, "DEV-TERM-01");
        }

        Shift openShift = shiftService.findOpenShift("seed-cashier", "DEV-TERM-01").orElse(null);
        if (openShift != null && openShift.getStatus() == ShiftStatus.OPEN) {
            ShiftService.ShiftReconciliationData preview = shiftService.previewReconciliation(openShift, Map.of());
            Map<String, BigDecimal> counted = new LinkedHashMap<>(preview.expectedByCurrency());
            if (counted.isEmpty()) {
                counted.put("USD", BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
            }
            shiftService.closeShift(
                    "seed-cashier",
                    "DEV-TERM-01",
                    counted,
                    "Auto-closed by dev seeder",
                    true
            );
        }
    }

    private void seedSuppliersAndReceiving() {
        Map<String, Supplier> suppliersByName = ensureSuppliers();
        if (purchaseOrderRepo.count() > 0 || goodsReceiptRepo.count() > 0) return;

        List<Product> products = productRepo.findAll();
        if (products.isEmpty()) return;

        Supplier alpha = suppliersByName.get("alpha distribution co.");
        Supplier beta = suppliersByName.get("beta fmcg supply");
        if (alpha == null || beta == null) return;

        PurchaseOrder po1 = new PurchaseOrder();
        po1.setSupplier(alpha);
        po1.setStatus(PurchaseOrderStatus.SENT);
        po1.setCreatedAt(LocalDateTime.now().minusDays(2));
        po1.setCreatedBy("seed-admin");
        po1.setExpectedAt(LocalDate.now().plusDays(2));
        po1.setCurrency("USD");
        po1.setNotes("Weekly replenishment");

        addPoItem(po1, products.get(0), 30, new BigDecimal("0.85"));
        if (products.size() > 1) {
            addPoItem(po1, products.get(1), 18, new BigDecimal("2.10"));
        }
        po1 = purchaseOrderRepo.save(po1);

        GoodsReceipt grn1 = new GoodsReceipt();
        grn1.setPurchaseOrder(po1);
        grn1.setReceivedAt(LocalDateTime.now().minusDays(1));
        grn1.setReceivedBy("seed-cashier");
        grn1.setInvoiceNo("ALPHA-INV-1001");
        grn1.setNotes("Partial delivery");
        addGrnItem(grn1, products.get(0), 20, new BigDecimal("0.85"));
        if (products.size() > 1) {
            addGrnItem(grn1, products.get(1), 8, new BigDecimal("2.10"));
        }
        grn1 = goodsReceiptRepo.save(grn1);
        applyGoodsReceipt(grn1, "DEV-TERM-01");
        purchaseOrderRepo.save(po1);

        PurchaseOrder po2 = new PurchaseOrder();
        po2.setSupplier(beta);
        po2.setStatus(PurchaseOrderStatus.SENT);
        po2.setCreatedAt(LocalDateTime.now().minusDays(3));
        po2.setCreatedBy("seed-admin");
        po2.setExpectedAt(LocalDate.now().plusDays(1));
        po2.setCurrency("USD");
        po2.setNotes("Bulk promo restock");
        Product target = products.size() > 2 ? products.get(2) : products.get(0);
        addPoItem(po2, target, 50, new BigDecimal("1.15"));
        po2 = purchaseOrderRepo.save(po2);

        GoodsReceipt grn2 = new GoodsReceipt();
        grn2.setPurchaseOrder(po2);
        grn2.setReceivedAt(LocalDateTime.now().minusHours(12));
        grn2.setReceivedBy("seed-cashier");
        grn2.setInvoiceNo("BETA-INV-2001");
        grn2.setNotes("Full delivery");
        addGrnItem(grn2, target, 50, new BigDecimal("1.15"));
        grn2 = goodsReceiptRepo.save(grn2);
        applyGoodsReceipt(grn2, "DEV-TERM-01");
        purchaseOrderRepo.save(po2);
    }

    private Map<String, Supplier> ensureSuppliers() {
        List<SupplierSeed> seeds = List.of(
                new SupplierSeed("Alpha Distribution Co.", "+1-555-1001", "sales@alpha-dist.example", "101 Warehouse Ave, Springfield"),
                new SupplierSeed("Beta FMCG Supply", "+1-555-2002", "orders@beta-fmcg.example", "82 Market Road, Shelbyville"),
                new SupplierSeed("Northline Wholesale", "+1-555-3003", "hello@northline.example", "44 Trade Center Blvd, Capital City"),
                new SupplierSeed("FreshMart Vendors", "+1-555-4004", "support@freshmart-vendors.example", "29 Orchard Lane, Ogdenville"),
                new SupplierSeed("Urban Essentials Trading", "+1-555-5005", "sales@urban-essentials.example", "900 Commerce Dr, North Haverbrook"),
                new SupplierSeed("Sunrise Beverage Partners", "+1-555-6006", "orders@sunrise-bev.example", "12 Bottling St, Springfield")
        );

        Map<String, Supplier> byName = new HashMap<>();
        for (Supplier supplier : supplierRepo.findAll()) {
            if (supplier.getName() == null) continue;
            byName.put(supplier.getName().trim().toLowerCase(), supplier);
        }

        List<Supplier> toCreate = new ArrayList<>();
        for (SupplierSeed seed : seeds) {
            String key = seed.name().trim().toLowerCase();
            if (byName.containsKey(key)) continue;
            Supplier supplier = new Supplier();
            supplier.setName(seed.name());
            supplier.setPhone(seed.phone());
            supplier.setEmail(seed.email());
            supplier.setAddress(seed.address());
            supplier.setStatus(SupplierStatus.ACTIVE);
            toCreate.add(supplier);
        }

        if (!toCreate.isEmpty()) {
            List<Supplier> saved = supplierRepo.saveAll(toCreate);
            for (Supplier supplier : saved) {
                byName.put(supplier.getName().trim().toLowerCase(), supplier);
            }
        }
        return byName;
    }

    private void applyGoodsReceipt(GoodsReceipt receipt, String terminalId) {
        if (receipt == null || receipt.getItems() == null || receipt.getItems().isEmpty()) return;

        PurchaseOrder po = receipt.getPurchaseOrder();
        Map<Long, Integer> receivedByProduct = new HashMap<>();
        for (GoodsReceiptItem item : receipt.getItems()) {
            if (item.getProduct() == null || item.getProduct().getId() == null) continue;
            int qty = item.getReceivedQty() == null ? 0 : item.getReceivedQty();
            if (qty <= 0) continue;

            stockMovementService.recordReceive(
                    item.getProduct().getId(),
                    qty,
                    item.getUnitCost(),
                    po == null ? null : po.getCurrency(),
                    "GRN",
                    String.valueOf(receipt.getId()),
                    terminalId,
                    "Dev seed receiving"
            );
            receivedByProduct.merge(item.getProduct().getId(), qty, Integer::sum);
        }

        if (po == null) return;
        for (PurchaseOrderItem poItem : po.getItems()) {
            if (poItem.getProduct() == null || poItem.getProduct().getId() == null) continue;
            int current = poItem.getReceivedQty() == null ? 0 : poItem.getReceivedQty();
            int delta = receivedByProduct.getOrDefault(poItem.getProduct().getId(), 0);
            poItem.setReceivedQty(current + delta);
        }
        po.setStatus(resolvePoStatus(po));
    }

    private PurchaseOrderStatus resolvePoStatus(PurchaseOrder po) {
        int ordered = 0;
        int received = 0;
        for (PurchaseOrderItem item : po.getItems()) {
            ordered += item.getOrderedQty() == null ? 0 : item.getOrderedQty();
            received += item.getReceivedQty() == null ? 0 : item.getReceivedQty();
        }
        if (ordered <= 0) return PurchaseOrderStatus.DRAFT;
        if (received <= 0) return PurchaseOrderStatus.SENT;
        if (received < ordered) return PurchaseOrderStatus.PARTIAL;
        return PurchaseOrderStatus.RECEIVED;
    }

    private void addPoItem(PurchaseOrder po, Product product, int orderedQty, BigDecimal unitCost) {
        PurchaseOrderItem item = new PurchaseOrderItem();
        item.setPurchaseOrder(po);
        item.setProduct(product);
        item.setOrderedQty(orderedQty);
        item.setReceivedQty(0);
        item.setUnitCost(unitCost.setScale(4, RoundingMode.HALF_UP));
        item.setTax(BigDecimal.ZERO.setScale(4, RoundingMode.HALF_UP));
        item.setDiscount(BigDecimal.ZERO.setScale(4, RoundingMode.HALF_UP));
        po.getItems().add(item);
    }

    private void addGrnItem(GoodsReceipt receipt, Product product, int receivedQty, BigDecimal unitCost) {
        GoodsReceiptItem item = new GoodsReceiptItem();
        item.setGoodsReceipt(receipt);
        item.setProduct(product);
        item.setReceivedQty(receivedQty);
        item.setUnitCost(unitCost.setScale(4, RoundingMode.HALF_UP));
        receipt.getItems().add(item);
    }

    private Category ensureCategory(String name, Map<String, Category> byName, List<Category> toCreate) {
        String key = name.toLowerCase();
        if (byName.containsKey(key)) return byName.get(key);
        Category c = new Category();
        c.setName(name);
        c.setActive(true);
        c.setDescription(name + " items and essentials.");
        c.setSortOrder(byName.size() + toCreate.size() + 1);
        byName.put(key, c);
        toCreate.add(c);
        return c;
    }

    private Product newProduct(ProductSeed seed) {
        Product p = new Product();
        p.setSku(seed.sku());
        p.setBarcode(seed.barcode());
        p.setName(seed.name());
        BigDecimal priceValue = new BigDecimal(seed.price());
        p.setPrice(priceValue);
        p.setWholesalePrice(priceValue.multiply(new BigDecimal("0.85")).setScale(2, RoundingMode.HALF_UP));
        p.setWholesaleMinQty(12);
        p.setCostPrice(priceValue.multiply(new BigDecimal("0.60")).setScale(2, RoundingMode.HALF_UP));
        p.setStockQty(0);
        p.setUnitsPerBox(6);
        p.setUnitsPerCase(24);
        p.setActive(true);
        p.setAllowNegativeStock(false);
        p.setCategory(seed.category());
        p.setImageUrl(seed.imageUrl());
        return p;
    }

    private record ProductSeed(String sku,
                               String barcode,
                               String name,
                               String price,
                               int stockQty,
                               Category category,
                               String imageUrl) {
    }

    private record SupplierSeed(String name, String phone, String email, String address) {
    }
}
