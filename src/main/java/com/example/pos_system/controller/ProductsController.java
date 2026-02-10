package com.example.pos_system.controller;

import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.Category;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.ProductRepo;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import jakarta.persistence.criteria.Predicate;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.*;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.UUID;

@Controller
@RequestMapping("/products")
public class ProductsController {
    private static final int PAGE_SIZE = 20;
    private final ProductRepo productRepo;
    private final CategoryRepo categoryRepo;

    public ProductsController(ProductRepo productRepo, CategoryRepo categoryRepo) {
        this.productRepo = productRepo;
        this.categoryRepo = categoryRepo;
    }

    @GetMapping
    public String list(@RequestParam(required = false) Long categoryId,
                       @RequestParam(required = false) Boolean lowStock,
                       @RequestParam(required = false) String q,
                       @RequestParam(required = false) Boolean active,
                       @RequestParam(required = false) BigDecimal priceMin,
                       @RequestParam(required = false) BigDecimal priceMax,
                       @RequestParam(required = false) Integer stockMin,
                       @RequestParam(required = false) Integer stockMax,
                       @RequestParam(required = false) String sort,
                       @RequestParam(defaultValue = "asc") String dir,
                       @RequestParam(required = false) String error,
                       @RequestParam(defaultValue = "0") int page,
                       Model model) {
        int pageNum = Math.max(0, page);
        boolean onlyLowStock = Boolean.TRUE.equals(lowStock);
        Sort sortSpec = buildSort(sort, dir);
        Pageable pageable = PageRequest.of(pageNum, PAGE_SIZE, sortSpec);
        Specification<Product> specification = buildSpecification(categoryId, onlyLowStock, q, active, priceMin, priceMax, stockMin, stockMax);

        Page<Product> productPage = productRepo.findAll(specification, pageable);
        List<Product> pageItems = productPage.getContent();

        model.addAttribute("products", pageItems);
        model.addAttribute("page", productPage.getNumber());
        model.addAttribute("totalPages", Math.max(1, productPage.getTotalPages()));
        model.addAttribute("hasNext", productPage.hasNext());
        model.addAttribute("hasPrev", productPage.hasPrevious());
        model.addAttribute("nextPage", productPage.getNumber() + 1);
        model.addAttribute("prevPage", Math.max(0, productPage.getNumber() - 1));
        List<Category> categories = categoryRepo.findAll(Sort.by("sortOrder").ascending().and(Sort.by("name").ascending()));
        model.addAttribute("categories", categories);
        model.addAttribute("categoryId", categoryId);
        model.addAttribute("lowStock", onlyLowStock);
        model.addAttribute("q", q);
        model.addAttribute("active", active);
        model.addAttribute("priceMin", priceMin);
        model.addAttribute("priceMax", priceMax);
        model.addAttribute("stockMin", stockMin);
        model.addAttribute("stockMax", stockMax);
        model.addAttribute("sort", sort);
        model.addAttribute("dir", dir);
        ProductListStats stats = buildProductListStats(pageItems, categories);
        model.addAttribute("productStats", stats);
        model.addAttribute("totalProducts", productRepo.count());
        model.addAttribute("filteredTotal", productPage.getTotalElements());
        model.addAttribute("lowStockCount", productRepo.countLowStock());
        if ("invalidImage".equals(error)) {
            model.addAttribute("error", "Please upload a valid image file.");
        }
        if ("uploadFailed".equals(error)) {
            model.addAttribute("error", "Image upload failed. Please try again.");
        }
        if ("imageUrlTooLong".equals(error)) {
            model.addAttribute("error", "Image URL is too long. Please use a shorter link.");
        }
        if ("duplicate".equals(error)) {
            model.addAttribute("error", "SKU or barcode already exists. Please use a unique value.");
        }
        return "products/list";
    }

    @GetMapping("/new")
    public String createForm(Model model) {
        Product product = new Product();
        model.addAttribute("product", product);
        model.addAttribute("categories", categoryRepo.findAll(Sort.by("sortOrder").ascending().and(Sort.by("name").ascending())));
        addProductAnalytics(model, product);
        return "products/form";
    }

    @GetMapping("/{id}/edit")
    public String editForm(@PathVariable Long id, Model model) {
        Product product = productRepo.findById(id).orElseThrow();
        model.addAttribute("product", product);
        model.addAttribute("categories", categoryRepo.findAll(Sort.by("sortOrder").ascending().and(Sort.by("name").ascending())));
        addProductAnalytics(model, product);
        return "products/form";
    }

    @PostMapping
    public String save(@ModelAttribute Product product,
                       @RequestParam(required = false) Long categoryId,
                       @RequestParam(required = false) MultipartFile imageFile) {
        normalizeEmptyStrings(product);
        normalizeNumbers(product);
        if (categoryId != null) {
            product.setCategory(categoryRepo.findById(categoryId).orElse(null));
        } else {
            product.setCategory(null);
        }
        if (product.getActive() == null) {
            product.setActive(false);
        }
        if (imageFile != null && !imageFile.isEmpty()) {
            if (imageFile.getContentType() == null || !imageFile.getContentType().startsWith("image/")) {
                return "redirect:/products?error=invalidImage";
            }
            String imageUrl = storeImage(imageFile);
            if (imageUrl == null) {
                return "redirect:/products?error=uploadFailed";
            }
            product.setImageUrl(imageUrl);
        } else if (product.getImageUrl() != null && product.getImageUrl().length() > 2048) {
            return "redirect:/products?error=imageUrlTooLong";
        }
        try {
            productRepo.save(product);
        } catch (DataIntegrityViolationException ex) {
            return "redirect:/products?error=duplicate";
        }
        return "redirect:/products";
    }

    @PostMapping("/{id}/delete")
    public String delete(@PathVariable Long id) {
        productRepo.deleteById(id);
        return "redirect:/products";
    }

    @PostMapping("/{id}/toggle-active")
    public String toggleActive(@PathVariable Long id,
                               @RequestParam(required = false) Long categoryId,
                               @RequestParam(required = false) Boolean lowStock,
                               @RequestParam(required = false) String q,
                               @RequestParam(required = false) Boolean active,
                               @RequestParam(required = false) BigDecimal priceMin,
                               @RequestParam(required = false) BigDecimal priceMax,
                               @RequestParam(required = false) Integer stockMin,
                               @RequestParam(required = false) Integer stockMax,
                               @RequestParam(required = false) String sort,
                               @RequestParam(required = false) String dir,
                               @RequestParam(required = false) Integer page) {
        Product product = productRepo.findById(id).orElseThrow();
        boolean nextActive = !Boolean.TRUE.equals(product.getActive());
        product.setActive(nextActive);
        productRepo.save(product);
        return "redirect:" + buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                stockMin, stockMax, sort, dir, page);
    }

    private String storeImage(MultipartFile imageFile) {
        String originalName = imageFile.getOriginalFilename();
        String ext = "";
        if (originalName != null) {
            int dot = originalName.lastIndexOf('.');
            if (dot >= 0) ext = originalName.substring(dot);
        }
        String filename = UUID.randomUUID().toString().replace("-", "") + ext;

        Path uploadDir = Paths.get("src/main/resources/static/uploads");
        Path targetDir = Paths.get("target/classes/static/uploads");
        try {
            Files.createDirectories(uploadDir);
            Path saved = uploadDir.resolve(filename);
            Files.copy(imageFile.getInputStream(), saved, StandardCopyOption.REPLACE_EXISTING);
            try {
                Files.createDirectories(targetDir);
                Files.copy(saved, targetDir.resolve(filename), StandardCopyOption.REPLACE_EXISTING);
            } catch (IOException ignored) {
                // best effort: keep dev-time live classpath folder updated
            }
        } catch (IOException e) {
            return null;
        }
        return "/uploads/" + filename;
    }

    private void normalizeEmptyStrings(Product product) {
        if (product.getSku() != null && product.getSku().trim().isEmpty()) {
            product.setSku(null);
        }
        if (product.getBarcode() != null && product.getBarcode().trim().isEmpty()) {
            product.setBarcode(null);
        }
        if (product.getImageUrl() != null && product.getImageUrl().trim().isEmpty()) {
            product.setImageUrl(null);
        }
        if (product.getName() != null && product.getName().trim().isEmpty()) {
            product.setName(null);
        }
    }

    private void normalizeNumbers(Product product) {
        if (product.getUnitsPerBox() != null && product.getUnitsPerBox() <= 0) {
            product.setUnitsPerBox(null);
        }
        if (product.getUnitsPerCase() != null && product.getUnitsPerCase() <= 0) {
            product.setUnitsPerCase(null);
        }
        if (product.getWholesaleMinQty() != null && product.getWholesaleMinQty() <= 0) {
            product.setWholesaleMinQty(null);
        }
    }

    private Specification<Product> buildSpecification(Long categoryId,
                                                      boolean onlyLowStock,
                                                      String q,
                                                      Boolean active,
                                                      BigDecimal priceMin,
                                                      BigDecimal priceMax,
                                                      Integer stockMin,
                                                      Integer stockMax) {
        return (root, query, cb) -> {
            List<Predicate> predicates = new java.util.ArrayList<>();
            if (categoryId != null) {
                predicates.add(cb.equal(root.get("category").get("id"), categoryId));
            }
            if (onlyLowStock) {
                predicates.add(cb.isNotNull(root.get("stockQty")));
                predicates.add(cb.isNotNull(root.get("lowStockThreshold")));
                predicates.add(cb.lessThanOrEqualTo(root.get("stockQty"), root.get("lowStockThreshold")));
            }
            if (active != null) {
                predicates.add(cb.equal(root.get("active"), active));
            }
            if (q != null && !q.isBlank()) {
                String like = "%" + q.trim().toLowerCase() + "%";
                predicates.add(cb.or(
                        cb.like(cb.lower(root.get("name")), like),
                        cb.like(cb.lower(root.get("sku")), like),
                        cb.like(cb.lower(root.get("barcode")), like)
                ));
            }
            if (priceMin != null || priceMax != null) {
                predicates.add(cb.isNotNull(root.get("price")));
                if (priceMin != null) {
                    predicates.add(cb.greaterThanOrEqualTo(root.get("price"), priceMin));
                }
                if (priceMax != null) {
                    predicates.add(cb.lessThanOrEqualTo(root.get("price"), priceMax));
                }
            }
            if (stockMin != null || stockMax != null) {
                predicates.add(cb.isNotNull(root.get("stockQty")));
                if (stockMin != null) {
                    predicates.add(cb.greaterThanOrEqualTo(root.get("stockQty"), stockMin));
                }
                if (stockMax != null) {
                    predicates.add(cb.lessThanOrEqualTo(root.get("stockQty"), stockMax));
                }
            }
            return cb.and(predicates.toArray(new Predicate[0]));
        };
    }

    private Sort buildSort(String sort, String dir) {
        Sort.Order order;
        if ("price".equalsIgnoreCase(sort)) {
            order = new Sort.Order(Sort.Direction.ASC, "price");
        } else if ("stock".equalsIgnoreCase(sort)) {
            order = new Sort.Order(Sort.Direction.ASC, "stockQty");
        } else if ("sku".equalsIgnoreCase(sort)) {
            order = new Sort.Order(Sort.Direction.ASC, "sku");
        } else {
            order = new Sort.Order(Sort.Direction.ASC, "name");
        }
        if ("desc".equalsIgnoreCase(dir)) {
            order = order.with(Sort.Direction.DESC);
        }
        return Sort.by(order);
    }

    private ProductListStats buildProductListStats(List<Product> products, List<Category> categories) {
        int total = products.size();
        long activeCount = 0;
        long lowStockCount = 0;
        long outOfStockCount = 0;
        long withSkuCount = 0;
        long withBarcodeCount = 0;
        long withImageCount = 0;
        long pricedCount = 0;
        long costCount = 0;
        long marginCount = 0;
        int stockUnits = 0;

        BigDecimal stockValueRetail = BigDecimal.ZERO;
        BigDecimal stockValueCost = BigDecimal.ZERO;
        BigDecimal potentialProfit = BigDecimal.ZERO;
        BigDecimal sumPrice = BigDecimal.ZERO;
        BigDecimal sumCost = BigDecimal.ZERO;
        BigDecimal sumMarginPct = BigDecimal.ZERO;
        BigDecimal minPrice = null;
        BigDecimal maxPrice = null;

        for (Product product : products) {
            if (Boolean.TRUE.equals(product.getActive())) activeCount++;
            if (product.isLowStock()) lowStockCount++;
            Integer stockQty = product.getStockQty();
            if (stockQty != null) {
                if (stockQty == 0) outOfStockCount++;
                if (stockQty > 0) stockUnits += stockQty;
            }
            if (hasText(product.getSku())) withSkuCount++;
            if (hasText(product.getBarcode())) withBarcodeCount++;
            if (hasText(product.getImageUrl())) withImageCount++;

            BigDecimal price = product.getPrice();
            BigDecimal cost = product.getCostPrice();
            if (price != null) {
                pricedCount++;
                sumPrice = sumPrice.add(price);
                minPrice = minPrice == null ? price : minPrice.min(price);
                maxPrice = maxPrice == null ? price : maxPrice.max(price);
                if (stockQty != null && stockQty > 0) {
                    stockValueRetail = stockValueRetail.add(price.multiply(BigDecimal.valueOf(stockQty)));
                }
            }
            if (cost != null) {
                costCount++;
                sumCost = sumCost.add(cost);
                if (stockQty != null && stockQty > 0) {
                    stockValueCost = stockValueCost.add(cost.multiply(BigDecimal.valueOf(stockQty)));
                }
            }
            if (price != null && cost != null && price.compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal margin = price.subtract(cost);
                BigDecimal marginPct = margin.divide(price, 6, RoundingMode.HALF_UP)
                        .multiply(new BigDecimal("100"));
                sumMarginPct = sumMarginPct.add(marginPct);
                marginCount++;
                if (stockQty != null && stockQty > 0) {
                    potentialProfit = potentialProfit.add(margin.multiply(BigDecimal.valueOf(stockQty)));
                }
            }
        }

        BigDecimal avgPrice = pricedCount > 0 ? sumPrice.divide(BigDecimal.valueOf(pricedCount), 2, RoundingMode.HALF_UP) : null;
        BigDecimal avgCost = costCount > 0 ? sumCost.divide(BigDecimal.valueOf(costCount), 2, RoundingMode.HALF_UP) : null;
        BigDecimal avgMarginPct = marginCount > 0
                ? sumMarginPct.divide(BigDecimal.valueOf(marginCount), 2, RoundingMode.HALF_UP)
                : null;

        Map<Long, String> categoryNames = new HashMap<>();
        for (Category category : categories) {
            categoryNames.put(category.getId(), category.getName());
        }

        List<ProductValue> topStockValue = topStockValue(products, 5);
        List<ProductMargin> topMargin = topMargin(products, 5);
        List<CategoryMetric> topCategories = topCategoryValue(products, categoryNames, 5);

        return new ProductListStats(
                total,
                activeCount,
                total - activeCount,
                lowStockCount,
                outOfStockCount,
                withSkuCount,
                withBarcodeCount,
                withImageCount,
                stockUnits,
                stockValueRetail,
                stockValueCost,
                potentialProfit,
                avgPrice,
                avgCost,
                avgMarginPct,
                minPrice,
                maxPrice,
                topStockValue,
                topMargin,
                topCategories
        );
    }

    private List<ProductValue> topStockValue(List<Product> products, int limit) {
        Comparator<ProductValue> comparator = Comparator.comparing(ProductValue::value);
        PriorityQueue<ProductValue> queue = new PriorityQueue<>(comparator);
        for (Product product : products) {
            BigDecimal price = product.getPrice();
            Integer stockQty = product.getStockQty();
            if (price == null || stockQty == null || stockQty <= 0) continue;
            BigDecimal value = price.multiply(BigDecimal.valueOf(stockQty));
            addTop(queue, new ProductValue(product.getId(), safeName(product), value, stockQty), limit, comparator);
        }
        return queue.stream()
                .sorted(comparator.reversed())
                .toList();
    }

    private List<ProductMargin> topMargin(List<Product> products, int limit) {
        Comparator<ProductMargin> comparator = Comparator.comparing(ProductMargin::margin);
        PriorityQueue<ProductMargin> queue = new PriorityQueue<>(comparator);
        for (Product product : products) {
            BigDecimal price = product.getPrice();
            BigDecimal cost = product.getCostPrice();
            if (price == null || cost == null || price.compareTo(BigDecimal.ZERO) <= 0) continue;
            BigDecimal margin = price.subtract(cost);
            BigDecimal marginPct = margin.divide(price, 6, RoundingMode.HALF_UP)
                    .multiply(new BigDecimal("100"));
            addTop(queue, new ProductMargin(product.getId(), safeName(product), margin, marginPct), limit, comparator);
        }
        return queue.stream()
                .sorted(comparator.reversed())
                .toList();
    }

    private List<CategoryMetric> topCategoryValue(List<Product> products, Map<Long, String> categoryNames, int limit) {
        Map<Long, CategoryMetricBuilder> map = new HashMap<>();
        for (Product product : products) {
            Integer stockQty = product.getStockQty();
            if (stockQty == null || stockQty <= 0) continue;
            BigDecimal price = product.getPrice();
            if (price == null) continue;
            Long categoryId = product.getCategory() == null ? null : product.getCategory().getId();
            String name = categoryNames.getOrDefault(categoryId, categoryId == null ? "Uncategorized" : "Unknown");
            CategoryMetricBuilder builder = map.computeIfAbsent(categoryId, id -> new CategoryMetricBuilder(name));
            builder.count++;
            builder.value = builder.value.add(price.multiply(BigDecimal.valueOf(stockQty)));
        }
        Comparator<CategoryMetric> comparator = Comparator.comparing(CategoryMetric::value);
        PriorityQueue<CategoryMetric> queue = new PriorityQueue<>(comparator);
        for (Map.Entry<Long, CategoryMetricBuilder> entry : map.entrySet()) {
            CategoryMetricBuilder builder = entry.getValue();
            addTop(queue, new CategoryMetric(builder.name, builder.value, builder.count), limit, comparator);
        }
        return queue.stream()
                .sorted(comparator.reversed())
                .toList();
    }

    private <T> void addTop(PriorityQueue<T> queue, T value, int limit, Comparator<T> comparator) {
        if (limit <= 0) return;
        if (queue.size() < limit) {
            queue.add(value);
            return;
        }
        T min = queue.peek();
        if (min != null && comparator.compare(value, min) > 0) {
            queue.poll();
            queue.add(value);
        }
    }

    private String safeName(Product product) {
        if (product == null) return "-";
        if (hasText(product.getName())) return product.getName();
        if (hasText(product.getSku())) return product.getSku();
        return "Product";
    }

    private boolean hasText(String value) {
        return value != null && !value.trim().isEmpty();
    }

    private void addProductAnalytics(Model model, Product product) {
        if (product == null) {
            model.addAttribute("productAnalytics", new ProductAnalytics(null, null, null, null, null, null, null, null, null, "Not tracked"));
            return;
        }
        BigDecimal price = product.getPrice();
        BigDecimal cost = product.getCostPrice();
        BigDecimal wholesale = product.getWholesalePrice();
        Integer stockQty = product.getStockQty();
        Integer lowStockThreshold = product.getLowStockThreshold();
        Integer unitsPerBox = product.getUnitsPerBox();
        Integer unitsPerCase = product.getUnitsPerCase();

        BigDecimal unitMargin = price != null && cost != null ? price.subtract(cost) : null;
        BigDecimal marginPct = price != null && cost != null && price.compareTo(BigDecimal.ZERO) > 0
                ? unitMargin.divide(price, 6, RoundingMode.HALF_UP).multiply(new BigDecimal("100"))
                : null;
        BigDecimal markupPct = price != null && cost != null && cost.compareTo(BigDecimal.ZERO) > 0
                ? unitMargin.divide(cost, 6, RoundingMode.HALF_UP).multiply(new BigDecimal("100"))
                : null;
        BigDecimal retailValue = price != null && stockQty != null
                ? price.multiply(BigDecimal.valueOf(stockQty))
                : null;
        BigDecimal costValue = cost != null && stockQty != null
                ? cost.multiply(BigDecimal.valueOf(stockQty))
                : null;
        BigDecimal potentialProfit = unitMargin != null && stockQty != null
                ? unitMargin.multiply(BigDecimal.valueOf(stockQty))
                : null;
        BigDecimal wholesaleDiscountPct = price != null && wholesale != null && price.compareTo(BigDecimal.ZERO) > 0
                ? price.subtract(wholesale).divide(price, 6, RoundingMode.HALF_UP).multiply(new BigDecimal("100"))
                : null;
        BigDecimal boxPrice = price != null && unitsPerBox != null && unitsPerBox > 0
                ? price.multiply(BigDecimal.valueOf(unitsPerBox))
                : null;
        BigDecimal casePrice = price != null && unitsPerCase != null && unitsPerCase > 0
                ? price.multiply(BigDecimal.valueOf(unitsPerCase))
                : null;

        String stockStatus = "Not tracked";
        if (stockQty != null) {
            if (stockQty == 0) {
                stockStatus = "Out of stock";
            } else if (lowStockThreshold != null && stockQty <= lowStockThreshold) {
                stockStatus = "Low stock";
            } else {
                stockStatus = "Healthy";
            }
        }

        model.addAttribute("productAnalytics", new ProductAnalytics(
                unitMargin,
                marginPct,
                markupPct,
                retailValue,
                costValue,
                potentialProfit,
                wholesaleDiscountPct,
                boxPrice,
                casePrice,
                stockStatus
        ));
    }

    private record ProductListStats(
            int total,
            long activeCount,
            long inactiveCount,
            long lowStockCount,
            long outOfStockCount,
            long withSkuCount,
            long withBarcodeCount,
            long withImageCount,
            int stockUnits,
            BigDecimal stockValueRetail,
            BigDecimal stockValueCost,
            BigDecimal potentialProfit,
            BigDecimal avgPrice,
            BigDecimal avgCost,
            BigDecimal avgMarginPct,
            BigDecimal minPrice,
            BigDecimal maxPrice,
            List<ProductValue> topStockValue,
            List<ProductMargin> topMargin,
            List<CategoryMetric> topCategories
    ) {}

    private record ProductValue(Long id, String name, BigDecimal value, Integer qty) {}

    private record ProductMargin(Long id, String name, BigDecimal margin, BigDecimal marginPct) {}

    private record CategoryMetric(String name, BigDecimal value, long count) {}

    private static class CategoryMetricBuilder {
        private final String name;
        private BigDecimal value = BigDecimal.ZERO;
        private long count = 0;

        private CategoryMetricBuilder(String name) {
            this.name = name;
        }
    }

    private record ProductAnalytics(
            BigDecimal unitMargin,
            BigDecimal marginPct,
            BigDecimal markupPct,
            BigDecimal retailValue,
            BigDecimal costValue,
            BigDecimal potentialProfit,
            BigDecimal wholesaleDiscountPct,
            BigDecimal boxPrice,
            BigDecimal casePrice,
            String stockStatus
    ) {}

    private String buildListRedirect(Long categoryId, Boolean lowStock, String q, Boolean active,
                                     BigDecimal priceMin, BigDecimal priceMax,
                                     Integer stockMin, Integer stockMax,
                                     String sort, String dir, Integer page) {
        StringBuilder redirect = new StringBuilder("/products");
        String sep = "?";
        if (categoryId != null) {
            redirect.append(sep).append("categoryId=").append(categoryId);
            sep = "&";
        }
        if (Boolean.TRUE.equals(lowStock)) {
            redirect.append(sep).append("lowStock=true");
            sep = "&";
        }
        if (q != null && !q.isBlank()) {
            redirect.append(sep).append("q=").append(java.net.URLEncoder.encode(q, java.nio.charset.StandardCharsets.UTF_8));
            sep = "&";
        }
        if (active != null) {
            redirect.append(sep).append("active=").append(active);
            sep = "&";
        }
        if (priceMin != null) {
            redirect.append(sep).append("priceMin=").append(priceMin);
            sep = "&";
        }
        if (priceMax != null) {
            redirect.append(sep).append("priceMax=").append(priceMax);
            sep = "&";
        }
        if (stockMin != null) {
            redirect.append(sep).append("stockMin=").append(stockMin);
            sep = "&";
        }
        if (stockMax != null) {
            redirect.append(sep).append("stockMax=").append(stockMax);
            sep = "&";
        }
        if (sort != null && !sort.isBlank()) {
            redirect.append(sep).append("sort=").append(sort);
            sep = "&";
        }
        if (dir != null && !dir.isBlank()) {
            redirect.append(sep).append("dir=").append(dir);
            sep = "&";
        }
        if (page != null) {
            redirect.append(sep).append("page=").append(page);
        }
        return redirect.toString();
    }
}
