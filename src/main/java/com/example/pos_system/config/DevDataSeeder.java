package com.example.pos_system.config;

import com.example.pos_system.entity.*;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.SaleRepo;

import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

@Component
@Profile("dev")
public class DevDataSeeder implements CommandLineRunner {
    private final CategoryRepo categoryRepo;
    private final ProductRepo productRepo;
    private final SaleRepo saleRepo;

    public DevDataSeeder(CategoryRepo categoryRepo, ProductRepo productRepo, SaleRepo saleRepo) {
        this.categoryRepo = categoryRepo;
        this.productRepo = productRepo;
        this.saleRepo = saleRepo;
    }

    @Override
    public void run(String... args) {
        seedCategoriesAndProducts();
        seedSampleSale();
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

        List<Product> products = new ArrayList<>();
        products.add(newProduct("BEV-001", "100000000001", "Cola 330ml", "1.50", 120, beverages,
                "https://images.unsplash.com/photo-1629203851122-3726ecdf080e?auto=format&fit=crop&w=200&q=80"));
        products.add(newProduct("BEV-002", "100000000002", "Orange Juice 1L", "3.20", 60, beverages,
                "https://images.unsplash.com/photo-1542444459-db0f86b5a7b2?auto=format&fit=crop&w=200&q=80"));
        products.add(newProduct("SNK-001", "100000000101", "Potato Chips", "2.10", 80, snacks,
                "https://images.unsplash.com/photo-1585238342028-4bbc3b8b2d8b?auto=format&fit=crop&w=200&q=80"));
        products.add(newProduct("SNK-002", "100000000102", "Chocolate Bar", "1.25", 150, snacks,
                "https://images.unsplash.com/photo-1541783245831-57d6fb0926d3?auto=format&fit=crop&w=200&q=80"));
        products.add(newProduct("ESS-001", "100000000201", "Hand Soap", "2.75", 40, essentials,
                "https://images.unsplash.com/photo-1583947215259-38e31be8751f?auto=format&fit=crop&w=200&q=80"));
        products.add(newProduct("ESS-002", "100000000202", "Paper Towels", "4.50", 30, essentials,
                "https://images.unsplash.com/photo-1614302264631-ef85d1793b79?auto=format&fit=crop&w=200&q=80"));

        productRepo.saveAll(products);
    }

    private void seedSampleSale() {
        if (saleRepo.count() > 0) return;
        List<Product> products = productRepo.findAll();
        if (products.isEmpty()) return;

        Product p1 = products.get(0);
        Product p2 = products.size() > 1 ? products.get(1) : p1;

        Sale sale = new Sale();
        sale.setCreatedAt(LocalDateTime.now().minusDays(1));
        sale.setPaymentMethod(PaymentMethod.CASH);
        sale.setStatus(SaleStatus.PAID);
        sale.setCashierUsername("seed");

        SaleItem item1 = new SaleItem();
        item1.setSale(sale);
        item1.setProduct(p1);
        item1.setQty(2);
        item1.setUnitPrice(p1.getPrice());
        item1.setLineTotal(p1.getPrice().multiply(new BigDecimal("2")));
        item1.setPriceTier(PriceTier.RETAIL);
        item1.setUnitType(UnitType.PIECE);
        item1.setUnitSize(1);

        SaleItem item2 = new SaleItem();
        item2.setSale(sale);
        item2.setProduct(p2);
        item2.setQty(1);
        item2.setUnitPrice(p2.getPrice());
        item2.setLineTotal(p2.getPrice());
        item2.setPriceTier(PriceTier.RETAIL);
        item2.setUnitType(UnitType.PIECE);
        item2.setUnitSize(1);

        sale.getItems().add(item1);
        sale.getItems().add(item2);

        BigDecimal subtotal = item1.getLineTotal().add(item2.getLineTotal());
        sale.setSubtotal(subtotal);
        sale.setDiscount(BigDecimal.ZERO);
        sale.setTax(BigDecimal.ZERO);
        sale.setTotal(subtotal);

        saleRepo.save(sale);
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

    private Product newProduct(String sku, String barcode, String name, String price, int stockQty, Category category, String imageUrl) {
        Product p = new Product();
        p.setSku(sku);
        p.setBarcode(barcode);
        p.setName(name);
        BigDecimal priceValue = new BigDecimal(price);
        p.setPrice(priceValue);
        p.setWholesalePrice(priceValue.multiply(new BigDecimal("0.85")).setScale(2, java.math.RoundingMode.HALF_UP));
        p.setWholesaleMinQty(12);
        p.setCostPrice(priceValue.multiply(new BigDecimal("0.60")).setScale(2, java.math.RoundingMode.HALF_UP));
        p.setStockQty(stockQty);
        p.setUnitsPerBox(6);
        p.setUnitsPerCase(24);
        p.setActive(true);
        p.setCategory(category);
        p.setImageUrl(imageUrl);
        return p;
    }
}
