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
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import org.springframework.transaction.annotation.Transactional;
import jakarta.persistence.criteria.Predicate;
import jakarta.servlet.http.HttpServletResponse;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.UUID;
import java.util.Collections;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

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

    @PostMapping("/import")
    public String importInventory(@RequestParam("file") MultipartFile file,
                                  @RequestParam(required = false, defaultValue = "false") boolean allowCreate,
                                  @RequestParam(required = false, defaultValue = "false") boolean createCategories,
                                  RedirectAttributes redirectAttributes) {
        if (file == null || file.isEmpty()) {
            redirectAttributes.addFlashAttribute("error", "Please choose a CSV or Excel file to import.");
            return "redirect:/products";
        }
        String filename = file.getOriginalFilename() == null ? "" : file.getOriginalFilename().toLowerCase();
        ImportResult result;
        try {
            if (filename.endsWith(".xlsx")) {
                result = importFromExcel(file, allowCreate, createCategories);
            } else if (filename.endsWith(".csv")) {
                result = importFromCsv(file, allowCreate, createCategories);
            } else {
                redirectAttributes.addFlashAttribute("error", "Unsupported file type. Please upload .csv or .xlsx.");
                return "redirect:/products";
            }
        } catch (IOException ex) {
            redirectAttributes.addFlashAttribute("error", "Import failed: " + ex.getMessage());
            return "redirect:/products";
        }

        if (result.errors != null && !result.errors.isEmpty()) {
            if (result.errors.size() > 10) {
                List<String> trimmed = new ArrayList<>(result.errors.subList(0, 10));
                trimmed.add("... and " + (result.errors.size() - 10) + " more.");
                redirectAttributes.addFlashAttribute("importErrors", trimmed);
            } else {
                redirectAttributes.addFlashAttribute("importErrors", result.errors);
            }
        }
        String summary = "Imported " + result.created + " created, " + result.updated + " updated, "
                + result.skipped + " skipped, " + result.failed + " failed.";
        redirectAttributes.addFlashAttribute("importSummary", summary);
        if (result.failed == 0) {
            redirectAttributes.addFlashAttribute("success", "Import completed. " + summary);
        } else {
            redirectAttributes.addFlashAttribute("error", "Import completed with some errors. " + summary);
        }
        return "redirect:/products";
    }

    @GetMapping("/export.csv")
    public void exportCsv(@RequestParam(required = false) Long categoryId,
                          @RequestParam(required = false) Boolean lowStock,
                          @RequestParam(required = false) String q,
                          @RequestParam(required = false) Boolean active,
                          @RequestParam(required = false) BigDecimal priceMin,
                          @RequestParam(required = false) BigDecimal priceMax,
                          @RequestParam(required = false) Integer stockMin,
                          @RequestParam(required = false) Integer stockMax,
                          @RequestParam(required = false) String sort,
                          @RequestParam(defaultValue = "asc") String dir,
                          HttpServletResponse response) throws IOException {
        List<Product> products = findFilteredProducts(categoryId, lowStock, q, active,
                priceMin, priceMax, stockMin, stockMax, sort, dir);

        response.setContentType("text/csv");
        response.setHeader("Content-Disposition", "attachment; filename=\"inventory-export.csv\"");

        try (PrintWriter writer = response.getWriter()) {
            writer.println("ID,Name,SKU,Barcode,Category,Price,Cost,Stock,LowStockThreshold,Active");
            for (Product product : products) {
                writer.println(buildCsvRow(product));
            }
        }
    }

    @GetMapping("/export.xlsx")
    public void exportExcel(@RequestParam(required = false) Long categoryId,
                            @RequestParam(required = false) Boolean lowStock,
                            @RequestParam(required = false) String q,
                            @RequestParam(required = false) Boolean active,
                            @RequestParam(required = false) BigDecimal priceMin,
                            @RequestParam(required = false) BigDecimal priceMax,
                            @RequestParam(required = false) Integer stockMin,
                            @RequestParam(required = false) Integer stockMax,
                            @RequestParam(required = false) String sort,
                            @RequestParam(defaultValue = "asc") String dir,
                            HttpServletResponse response) throws IOException {
        List<Product> products = findFilteredProducts(categoryId, lowStock, q, active,
                priceMin, priceMax, stockMin, stockMax, sort, dir);

        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=\"inventory-export.xlsx\"");

        try (Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("Inventory");
            String[] headers = new String[] {
                    "ID", "Name", "SKU", "Barcode", "Category", "Price", "Cost", "Stock", "Low Stock", "Active"
            };
            Row header = sheet.createRow(0);
            CellStyle headerStyle = workbook.createCellStyle();
            Font headerFont = workbook.createFont();
            headerFont.setBold(true);
            headerStyle.setFont(headerFont);

            for (int i = 0; i < headers.length; i++) {
                var cell = header.createCell(i);
                cell.setCellValue(headers[i]);
                cell.setCellStyle(headerStyle);
            }

            int rowIdx = 1;
            for (Product product : products) {
                Row row = sheet.createRow(rowIdx++);
                row.createCell(0).setCellValue(product.getId() == null ? "" : String.valueOf(product.getId()));
                row.createCell(1).setCellValue(nullToEmpty(product.getName()));
                row.createCell(2).setCellValue(nullToEmpty(product.getSku()));
                row.createCell(3).setCellValue(nullToEmpty(product.getBarcode()));
                row.createCell(4).setCellValue(product.getCategory() != null ? nullToEmpty(product.getCategory().getName()) : "Uncategorized");
                row.createCell(5).setCellValue(safeAmount(product.getPrice()).doubleValue());
                row.createCell(6).setCellValue(safeAmount(product.getCostPrice()).doubleValue());
                row.createCell(7).setCellValue(product.getStockQty() == null ? "" : String.valueOf(product.getStockQty()));
                row.createCell(8).setCellValue(product.getLowStockThreshold() == null ? "" : String.valueOf(product.getLowStockThreshold()));
                row.createCell(9).setCellValue(Boolean.TRUE.equals(product.getActive()) ? "Active" : "Inactive");
            }

            for (int i = 0; i < headers.length; i++) {
                sheet.autoSizeColumn(i);
            }

            workbook.write(response.getOutputStream());
        }
    }

    @PostMapping("/{id}/quick-update")
    public String quickUpdate(@PathVariable Long id,
                              @RequestParam(required = false) String price,
                              @RequestParam(required = false) String stockQty,
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
                              @RequestParam(required = false) Integer page,
                              RedirectAttributes redirectAttributes) {
        Product product = productRepo.findById(id).orElseThrow();
        boolean changed = false;

        if (hasText(price)) {
            BigDecimal parsedPrice = parseBigDecimal(price);
            if (parsedPrice == null || parsedPrice.compareTo(BigDecimal.ZERO) < 0) {
                redirectAttributes.addFlashAttribute("error", "Invalid price value.");
                return "redirect:" + buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                        stockMin, stockMax, sort, dir, page);
            }
            product.setPrice(parsedPrice);
            changed = true;
        }

        if (hasText(stockQty)) {
            Integer parsedStock = parseInteger(stockQty);
            if (parsedStock == null || parsedStock < 0) {
                redirectAttributes.addFlashAttribute("error", "Invalid stock quantity.");
                return "redirect:" + buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                        stockMin, stockMax, sort, dir, page);
            }
            product.setStockQty(parsedStock);
            changed = true;
        }

        if (!changed) {
            redirectAttributes.addFlashAttribute("error", "No changes provided for quick update.");
            return "redirect:" + buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                    stockMin, stockMax, sort, dir, page);
        }

        productRepo.save(product);
        redirectAttributes.addFlashAttribute("success", "Updated " + safeName(product) + ".");
        return "redirect:" + buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                stockMin, stockMax, sort, dir, page);
    }

    @PostMapping("/bulk-stock")
    @Transactional
    public String bulkStockAdjust(@RequestParam(required = false) List<Long> ids,
                                  @RequestParam(required = false) String operation,
                                  @RequestParam(required = false) String qty,
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
                                  @RequestParam(required = false) Integer page,
                                  RedirectAttributes redirectAttributes) {
        if (ids == null || ids.isEmpty()) {
            redirectAttributes.addFlashAttribute("error", "Select at least one product for bulk update.");
            return "redirect:" + buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                    stockMin, stockMax, sort, dir, page);
        }

        Integer qtyValue = parseInteger(qty);
        if (qtyValue == null || qtyValue <= 0) {
            redirectAttributes.addFlashAttribute("error", "Bulk quantity must be a positive number.");
            return "redirect:" + buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                    stockMin, stockMax, sort, dir, page);
        }

        int delta = "remove".equalsIgnoreCase(operation) ? -qtyValue : qtyValue;
        List<Product> products = productRepo.findAllById(ids);
        for (Product product : products) {
            Integer current = product.getStockQty();
            int next = (current == null ? 0 : current) + delta;
            if (next < 0) next = 0;
            product.setStockQty(next);
        }
        productRepo.saveAll(products);
        redirectAttributes.addFlashAttribute("success", "Adjusted stock for " + products.size() + " products.");
        return "redirect:" + buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                stockMin, stockMax, sort, dir, page);
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

    private List<Product> findFilteredProducts(Long categoryId,
                                               Boolean lowStock,
                                               String q,
                                               Boolean active,
                                               BigDecimal priceMin,
                                               BigDecimal priceMax,
                                               Integer stockMin,
                                               Integer stockMax,
                                               String sort,
                                               String dir) {
        boolean onlyLowStock = Boolean.TRUE.equals(lowStock);
        Sort sortSpec = buildSort(sort, dir);
        Specification<Product> specification = buildSpecification(categoryId, onlyLowStock, q, active, priceMin, priceMax, stockMin, stockMax);
        return productRepo.findAll(specification, sortSpec);
    }

    private String buildCsvRow(Product product) {
        String category = product.getCategory() != null ? nullToEmpty(product.getCategory().getName()) : "Uncategorized";
        return String.join(",",
                csv(product.getId() == null ? "" : String.valueOf(product.getId())),
                csv(nullToEmpty(product.getName())),
                csv(nullToEmpty(product.getSku())),
                csv(nullToEmpty(product.getBarcode())),
                csv(category),
                csv(safeAmount(product.getPrice()).toPlainString()),
                csv(safeAmount(product.getCostPrice()).toPlainString()),
                csv(product.getStockQty() == null ? "" : String.valueOf(product.getStockQty())),
                csv(product.getLowStockThreshold() == null ? "" : String.valueOf(product.getLowStockThreshold())),
                csv(Boolean.TRUE.equals(product.getActive()) ? "Active" : "Inactive")
        );
    }

    private String csv(String value) {
        if (value == null) return "";
        String escaped = value.replace("\"", "\"\"");
        if (escaped.contains(",") || escaped.contains("\"") || escaped.contains("\n")) {
            return "\"" + escaped + "\"";
        }
        return escaped;
    }

    private BigDecimal safeAmount(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    private String nullToEmpty(String value) {
        return value == null ? "" : value;
    }

    private ImportResult importFromCsv(MultipartFile file, boolean allowCreate, boolean createCategories) throws IOException {
        ImportResult result = new ImportResult();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(file.getInputStream(), StandardCharsets.UTF_8))) {
            String headerLine = reader.readLine();
            if (headerLine == null) {
                result.errors.add("Empty CSV file.");
                result.failed = 1;
                return result;
            }
            Map<String, Integer> headerIndex = buildHeaderIndex(parseCsvLine(headerLine));
            if (headerIndex.isEmpty()) {
                result.errors.add("No recognizable columns in CSV header.");
                result.failed = 1;
                return result;
            }
            String line;
            int rowNum = 1;
            while ((line = reader.readLine()) != null) {
                rowNum++;
                if (!hasText(line)) continue;
                List<String> values = parseCsvLine(line);
                Map<String, String> row = mapRow(values, headerIndex);
                ImportOutcome outcome = applyImportRow(row, allowCreate, createCategories, result.errors, rowNum);
                result.increment(outcome);
            }
        }
        return result;
    }

    private ImportResult importFromExcel(MultipartFile file, boolean allowCreate, boolean createCategories) throws IOException {
        ImportResult result = new ImportResult();
        try (Workbook workbook = new XSSFWorkbook(file.getInputStream())) {
            if (workbook.getNumberOfSheets() == 0) {
                result.errors.add("Excel file has no sheets.");
                result.failed = 1;
                return result;
            }
            Sheet sheet = workbook.getSheetAt(0);
            if (sheet == null) {
                result.errors.add("Excel file has no readable sheet.");
                result.failed = 1;
                return result;
            }
            Row headerRow = sheet.getRow(0);
            if (headerRow == null) {
                result.errors.add("Excel header row is missing.");
                result.failed = 1;
                return result;
            }
            DataFormatter formatter = new DataFormatter();
            List<String> headers = new ArrayList<>();
            int lastCell = headerRow.getLastCellNum();
            if (lastCell <= 0) {
                result.errors.add("Excel header row is empty.");
                result.failed = 1;
                return result;
            }
            for (int c = 0; c < lastCell; c++) {
                headers.add(formatter.formatCellValue(headerRow.getCell(c)));
            }
            Map<String, Integer> headerIndex = buildHeaderIndex(headers);
            if (headerIndex.isEmpty()) {
                result.errors.add("No recognizable columns in Excel header.");
                result.failed = 1;
                return result;
            }
            int lastRow = sheet.getLastRowNum();
            for (int r = 1; r <= lastRow; r++) {
                Row row = sheet.getRow(r);
                if (row == null) continue;
                Map<String, String> rowMap = new HashMap<>();
                for (Map.Entry<String, Integer> entry : headerIndex.entrySet()) {
                    int idx = entry.getValue();
                    rowMap.put(entry.getKey(), formatter.formatCellValue(row.getCell(idx)));
                }
                if (rowMap.values().stream().allMatch(v -> !hasText(v))) continue;
                ImportOutcome outcome = applyImportRow(rowMap, allowCreate, createCategories, result.errors, r + 1);
                result.increment(outcome);
            }
        }
        return result;
    }

    private ImportOutcome applyImportRow(Map<String, String> row,
                                         boolean allowCreate,
                                         boolean createCategories,
                                         List<String> errors,
                                         int rowNum) {
        if (row.values().stream().allMatch(v -> !hasText(v))) {
            return ImportOutcome.SKIPPED;
        }

        Long id = parseLong(row.get("id"));
        String sku = trimToNull(row.get("sku"));
        String barcode = trimToNull(row.get("barcode"));
        String name = trimToNull(row.get("name"));

        Product product = null;
        if (id != null) {
            product = productRepo.findById(id).orElse(null);
        }
        if (product == null && hasText(sku)) {
            product = productRepo.findBySkuIgnoreCase(sku).orElse(null);
        }
        if (product == null && hasText(barcode)) {
            product = productRepo.findByBarcode(barcode).orElse(null);
        }

        boolean created = false;
        if (product == null) {
            if (!allowCreate) {
                return ImportOutcome.SKIPPED;
            }
            if (!hasText(name)) {
                errors.add("Row " + rowNum + ": name is required for new products.");
                return ImportOutcome.FAILED;
            }
            product = new Product();
            product.setName(name);
            created = true;
        }

        if (hasText(name)) product.setName(name);
        if (hasText(sku)) product.setSku(sku);
        if (hasText(barcode)) product.setBarcode(barcode);

        String priceRaw = row.get("price");
        if (hasText(priceRaw)) {
            BigDecimal price = parseBigDecimal(priceRaw);
            if (price == null) {
                errors.add("Row " + rowNum + ": invalid price.");
                return ImportOutcome.FAILED;
            }
            if (price.compareTo(BigDecimal.ZERO) < 0) {
                errors.add("Row " + rowNum + ": price cannot be negative.");
                return ImportOutcome.FAILED;
            }
            product.setPrice(price);
        }
        String costRaw = row.get("costPrice");
        if (hasText(costRaw)) {
            BigDecimal cost = parseBigDecimal(costRaw);
            if (cost == null) {
                errors.add("Row " + rowNum + ": invalid cost price.");
                return ImportOutcome.FAILED;
            }
            if (cost.compareTo(BigDecimal.ZERO) < 0) {
                errors.add("Row " + rowNum + ": cost price cannot be negative.");
                return ImportOutcome.FAILED;
            }
            product.setCostPrice(cost);
        }
        String wholesaleRaw = row.get("wholesalePrice");
        if (hasText(wholesaleRaw)) {
            BigDecimal wholesale = parseBigDecimal(wholesaleRaw);
            if (wholesale == null) {
                errors.add("Row " + rowNum + ": invalid wholesale price.");
                return ImportOutcome.FAILED;
            }
            if (wholesale.compareTo(BigDecimal.ZERO) < 0) {
                errors.add("Row " + rowNum + ": wholesale price cannot be negative.");
                return ImportOutcome.FAILED;
            }
            product.setWholesalePrice(wholesale);
        }

        String stockRaw = row.get("stockQty");
        if (hasText(stockRaw)) {
            Integer stockQty = parseInteger(stockRaw);
            if (stockQty == null) {
                errors.add("Row " + rowNum + ": invalid stock quantity.");
                return ImportOutcome.FAILED;
            }
            if (stockQty < 0) {
                errors.add("Row " + rowNum + ": stock quantity cannot be negative.");
                return ImportOutcome.FAILED;
            }
            product.setStockQty(stockQty);
        }
        String lowStockRaw = row.get("lowStockThreshold");
        if (hasText(lowStockRaw)) {
            Integer lowStock = parseInteger(lowStockRaw);
            if (lowStock == null) {
                errors.add("Row " + rowNum + ": invalid low stock threshold.");
                return ImportOutcome.FAILED;
            }
            if (lowStock < 0) {
                errors.add("Row " + rowNum + ": low stock threshold cannot be negative.");
                return ImportOutcome.FAILED;
            }
            product.setLowStockThreshold(lowStock);
        }
        String wholesaleMinRaw = row.get("wholesaleMinQty");
        if (hasText(wholesaleMinRaw)) {
            Integer wholesaleMinQty = parseInteger(wholesaleMinRaw);
            if (wholesaleMinQty == null) {
                errors.add("Row " + rowNum + ": invalid wholesale min qty.");
                return ImportOutcome.FAILED;
            }
            if (wholesaleMinQty < 0) {
                errors.add("Row " + rowNum + ": wholesale min qty cannot be negative.");
                return ImportOutcome.FAILED;
            }
            if (wholesaleMinQty == 0) {
                product.setWholesaleMinQty(null);
            } else {
                product.setWholesaleMinQty(wholesaleMinQty);
            }
        }
        String unitsPerBoxRaw = row.get("unitsPerBox");
        if (hasText(unitsPerBoxRaw)) {
            Integer unitsPerBox = parseInteger(unitsPerBoxRaw);
            if (unitsPerBox == null) {
                errors.add("Row " + rowNum + ": invalid units per box.");
                return ImportOutcome.FAILED;
            }
            if (unitsPerBox < 0) {
                errors.add("Row " + rowNum + ": units per box cannot be negative.");
                return ImportOutcome.FAILED;
            }
            if (unitsPerBox == 0) {
                product.setUnitsPerBox(null);
            } else {
                product.setUnitsPerBox(unitsPerBox);
            }
        }
        String unitsPerCaseRaw = row.get("unitsPerCase");
        if (hasText(unitsPerCaseRaw)) {
            Integer unitsPerCase = parseInteger(unitsPerCaseRaw);
            if (unitsPerCase == null) {
                errors.add("Row " + rowNum + ": invalid units per case.");
                return ImportOutcome.FAILED;
            }
            if (unitsPerCase < 0) {
                errors.add("Row " + rowNum + ": units per case cannot be negative.");
                return ImportOutcome.FAILED;
            }
            if (unitsPerCase == 0) {
                product.setUnitsPerCase(null);
            } else {
                product.setUnitsPerCase(unitsPerCase);
            }
        }

        String activeRaw = row.get("active");
        if (hasText(activeRaw)) {
            Boolean active = parseBoolean(activeRaw);
            if (active == null) {
                errors.add("Row " + rowNum + ": invalid active value.");
                return ImportOutcome.FAILED;
            }
            product.setActive(active);
        }

        String categoryName = trimToNull(row.get("category"));
        if (hasText(categoryName)) {
            Category category = categoryRepo.findByNameIgnoreCase(categoryName).orElse(null);
            if (category == null && createCategories) {
                category = new Category();
                category.setName(categoryName);
                categoryRepo.save(category);
            }
            if (category != null) {
                product.setCategory(category);
            } else {
                errors.add("Row " + rowNum + ": category not found: " + categoryName);
            }
        }

        try {
            productRepo.save(product);
        } catch (DataIntegrityViolationException ex) {
            errors.add("Row " + rowNum + ": duplicate SKU or barcode.");
            return ImportOutcome.FAILED;
        }

        return created ? ImportOutcome.CREATED : ImportOutcome.UPDATED;
    }

    private Map<String, Integer> buildHeaderIndex(List<String> headers) {
        Map<String, Integer> map = new HashMap<>();
        for (int i = 0; i < headers.size(); i++) {
            String normalized = normalizeHeader(headers.get(i));
            String key = mapHeader(normalized);
            if (key != null && !map.containsKey(key)) {
                map.put(key, i);
            }
        }
        return map;
    }

    private Map<String, String> mapRow(List<String> values, Map<String, Integer> headerIndex) {
        Map<String, String> row = new HashMap<>();
        for (Map.Entry<String, Integer> entry : headerIndex.entrySet()) {
            int idx = entry.getValue();
            row.put(entry.getKey(), idx < values.size() ? values.get(idx) : "");
        }
        return row;
    }

    private String normalizeHeader(String header) {
        if (header == null) return "";
        return header.trim().toLowerCase().replaceAll("[^a-z0-9]", "");
    }

    private String mapHeader(String normalized) {
        return switch (normalized) {
            case "id" -> "id";
            case "sku" -> "sku";
            case "barcode" -> "barcode";
            case "name", "productname" -> "name";
            case "price", "unitprice" -> "price";
            case "cost", "costprice" -> "costPrice";
            case "stock", "stockqty", "qty", "quantity" -> "stockQty";
            case "lowstock", "lowstockthreshold", "reorderlevel" -> "lowStockThreshold";
            case "active", "enabled" -> "active";
            case "category", "categoryname" -> "category";
            case "wholesaleprice" -> "wholesalePrice";
            case "wholesaleminqty", "wholesalemin", "wholesaleminquantity" -> "wholesaleMinQty";
            case "unitsperbox", "boxqty" -> "unitsPerBox";
            case "unitspercase", "caseqty" -> "unitsPerCase";
            default -> null;
        };
    }

    private List<String> parseCsvLine(String line) {
        if (line == null) return Collections.emptyList();
        List<String> values = new ArrayList<>();
        StringBuilder sb = new StringBuilder();
        boolean inQuotes = false;
        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);
            if (ch == '"') {
                if (inQuotes && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    sb.append('"');
                    i++;
                } else {
                    inQuotes = !inQuotes;
                }
            } else if (ch == ',' && !inQuotes) {
                values.add(sb.toString());
                sb.setLength(0);
            } else {
                sb.append(ch);
            }
        }
        values.add(sb.toString());
        return values;
    }

    private Integer parseInteger(String value) {
        if (!hasText(value)) return null;
        try {
            return Integer.parseInt(value.trim().replace(",", ""));
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    private Long parseLong(String value) {
        if (!hasText(value)) return null;
        try {
            return Long.parseLong(value.trim().replace(",", ""));
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    private BigDecimal parseBigDecimal(String value) {
        if (!hasText(value)) return null;
        try {
            return new BigDecimal(value.trim().replace(",", ""));
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    private Boolean parseBoolean(String value) {
        if (!hasText(value)) return null;
        String normalized = value.trim().toLowerCase();
        if (normalized.equals("true") || normalized.equals("yes") || normalized.equals("1") || normalized.equals("active")) {
            return true;
        }
        if (normalized.equals("false") || normalized.equals("no") || normalized.equals("0") || normalized.equals("inactive")) {
            return false;
        }
        return null;
    }

    private String trimToNull(String value) {
        if (value == null) return null;
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private enum ImportOutcome {
        CREATED,
        UPDATED,
        SKIPPED,
        FAILED
    }

    private static class ImportResult {
        private int created;
        private int updated;
        private int skipped;
        private int failed;
        private final List<String> errors = new ArrayList<>();

        private void increment(ImportOutcome outcome) {
            if (outcome == null) return;
            switch (outcome) {
                case CREATED -> created++;
                case UPDATED -> updated++;
                case SKIPPED -> skipped++;
                case FAILED -> failed++;
            }
        }
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
