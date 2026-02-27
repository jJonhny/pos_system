package com.example.pos_system.controller;

import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.Category;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.service.InventoryService;

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
import jakarta.persistence.criteria.Predicate;
import jakarta.servlet.http.HttpServletResponse;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeParseException;
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
    private static final int DEFAULT_PAGE_SIZE = 20;
    private static final int MAX_PAGE_SIZE = 200;
    private final ProductRepo productRepo;
    private final CategoryRepo categoryRepo;
    private final InventoryService inventoryService;

    /**
     * Executes the ProductsController operation.
     * <p>Return value: A fully initialized ProductsController instance.</p>
     *
     * @param productRepo Parameter of type {@code ProductRepo} used by this operation.
     * @param categoryRepo Parameter of type {@code CategoryRepo} used by this operation.
     * @param inventoryService Parameter of type {@code InventoryService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ProductsController(ProductRepo productRepo, CategoryRepo categoryRepo, InventoryService inventoryService) {
        this.productRepo = productRepo;
        this.categoryRepo = categoryRepo;
        this.inventoryService = inventoryService;
    }

    /**
     * Executes the list operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param error Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code int} used by this operation.
     * @param size Parameter of type {@code int} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the list operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param error Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code int} used by this operation.
     * @param size Parameter of type {@code int} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the list operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param error Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code int} used by this operation.
     * @param size Parameter of type {@code int} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
                       @RequestParam(defaultValue = "20") int size,
                       Model model) {
        int pageNum = Math.max(0, page);
        int pageSize = normalizePageSize(size);
        boolean onlyLowStock = Boolean.TRUE.equals(lowStock);
        Sort sortSpec = buildSort(sort, dir);
        Pageable pageable = PageRequest.of(pageNum, pageSize, sortSpec);
        Specification<Product> specification = buildSpecification(categoryId, onlyLowStock, q, active, priceMin, priceMax, stockMin, stockMax);

        Page<Product> productPage = productRepo.findAll(specification, pageable);
        if (productPage.getTotalPages() > 0 && pageNum >= productPage.getTotalPages()) {
            int lastPage = Math.max(0, productPage.getTotalPages() - 1);
            String redirect = buildListRedirect(categoryId, onlyLowStock, q, active, priceMin, priceMax,
                    stockMin, stockMax, sort, dir, lastPage, pageSize);
            if (hasText(error)) {
                redirect = appendErrorCode(redirect, error);
            }
            return "redirect:" + redirect;
        }
        List<Product> pageItems = productPage.getContent();
        int totalPages = Math.max(1, productPage.getTotalPages());
        int startPage = Math.max(0, productPage.getNumber() - 2);
        int endPage = Math.min(totalPages - 1, productPage.getNumber() + 2);

        model.addAttribute("products", pageItems);
        model.addAttribute("page", productPage.getNumber());
        model.addAttribute("size", pageSize);
        model.addAttribute("totalPages", totalPages);
        model.addAttribute("startPage", startPage);
        model.addAttribute("endPage", endPage);
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
        model.addAttribute("totalProducts", productRepo.count(buildSpecification(null, false, null, null, null, null, null, null)));
        model.addAttribute("filteredTotal", productPage.getTotalElements());
        if ("invalidImage".equals(error)) {
            model.addAttribute("error", "Please upload a valid image file.");
        }
        if ("uploadFailed".equals(error)) {
            model.addAttribute("error", "Image upload failed. Please try again.");
        }
        if ("uploadTooLarge".equals(error)) {
            model.addAttribute("error", "Image is too large. Maximum upload size is 10MB.");
        }
        if ("imageUrlTooLong".equals(error)) {
            model.addAttribute("error", "Image URL is too long. Please use a shorter link.");
        }
        if ("invalidStock".equals(error)) {
            model.addAttribute("error", "Stock value is invalid or would break stock rules.");
        }
        if ("invalidDateRange".equals(error)) {
            model.addAttribute("error", "Expiration date must be on or after manufacture date.");
        }
        if ("notFound".equals(error)) {
            model.addAttribute("error", "Product not found.");
        }
        if ("duplicate".equals(error)) {
            model.addAttribute("error", "SKU or barcode already exists. Please use a unique value.");
        }
        return "products/list";
    }

    /**
     * Executes the createForm operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createForm operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createForm operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/new")
    public String createForm(@RequestParam(required = false) Long categoryId,
                             @RequestParam(required = false) Boolean lowStock,
                             @RequestParam(required = false) String q,
                             @RequestParam(required = false) Boolean active,
                             @RequestParam(required = false) BigDecimal priceMin,
                             @RequestParam(required = false) BigDecimal priceMax,
                             @RequestParam(required = false) Integer stockMin,
                             @RequestParam(required = false) Integer stockMax,
                             @RequestParam(required = false) String sort,
                             @RequestParam(defaultValue = "asc") String dir,
                             @RequestParam(defaultValue = "0") Integer page,
                             @RequestParam(defaultValue = "20") Integer size,
                             Model model) {
        Product product = new Product();
        product.setStockQty(0);
        model.addAttribute("product", product);
        model.addAttribute("categories", categoryRepo.findAll(Sort.by("sortOrder").ascending().and(Sort.by("name").ascending())));
        addProductAnalytics(model, product);
        addReturnState(model, categoryId, lowStock, q, active, priceMin, priceMax, stockMin, stockMax, sort, dir, page, size);
        return "products/form";
    }

    /**
     * Executes the editForm operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the editForm operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the editForm operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/{id}/edit")
    public String editForm(@PathVariable Long id,
                           @RequestParam(required = false) Long categoryId,
                           @RequestParam(required = false) Boolean lowStock,
                           @RequestParam(required = false) String q,
                           @RequestParam(required = false) Boolean active,
                           @RequestParam(required = false) BigDecimal priceMin,
                           @RequestParam(required = false) BigDecimal priceMax,
                           @RequestParam(required = false) Integer stockMin,
                           @RequestParam(required = false) Integer stockMax,
                           @RequestParam(required = false) String sort,
                           @RequestParam(defaultValue = "asc") String dir,
                           @RequestParam(defaultValue = "0") Integer page,
                           @RequestParam(defaultValue = "20") Integer size,
                           Model model) {
        Product product = productRepo.findById(id).orElseThrow();
        model.addAttribute("product", product);
        model.addAttribute("categories", categoryRepo.findAll(Sort.by("sortOrder").ascending().and(Sort.by("name").ascending())));
        addProductAnalytics(model, product);
        addReturnState(model, categoryId, lowStock, q, active, priceMin, priceMax, stockMin, stockMax, sort, dir, page, size);
        return "products/form";
    }

    /**
     * Executes the save operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param imageFile Parameter of type {@code MultipartFile} used by this operation.
     * @param returnCategoryId Parameter of type {@code Long} used by this operation.
     * @param returnLowStock Parameter of type {@code Boolean} used by this operation.
     * @param returnQ Parameter of type {@code String} used by this operation.
     * @param returnActive Parameter of type {@code Boolean} used by this operation.
     * @param returnPriceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param returnPriceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param returnStockMin Parameter of type {@code Integer} used by this operation.
     * @param returnStockMax Parameter of type {@code Integer} used by this operation.
     * @param returnSort Parameter of type {@code String} used by this operation.
     * @param returnDir Parameter of type {@code String} used by this operation.
     * @param returnPage Parameter of type {@code Integer} used by this operation.
     * @param returnSize Parameter of type {@code Integer} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the save operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param imageFile Parameter of type {@code MultipartFile} used by this operation.
     * @param returnCategoryId Parameter of type {@code Long} used by this operation.
     * @param returnLowStock Parameter of type {@code Boolean} used by this operation.
     * @param returnQ Parameter of type {@code String} used by this operation.
     * @param returnActive Parameter of type {@code Boolean} used by this operation.
     * @param returnPriceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param returnPriceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param returnStockMin Parameter of type {@code Integer} used by this operation.
     * @param returnStockMax Parameter of type {@code Integer} used by this operation.
     * @param returnSort Parameter of type {@code String} used by this operation.
     * @param returnDir Parameter of type {@code String} used by this operation.
     * @param returnPage Parameter of type {@code Integer} used by this operation.
     * @param returnSize Parameter of type {@code Integer} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the save operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param imageFile Parameter of type {@code MultipartFile} used by this operation.
     * @param returnCategoryId Parameter of type {@code Long} used by this operation.
     * @param returnLowStock Parameter of type {@code Boolean} used by this operation.
     * @param returnQ Parameter of type {@code String} used by this operation.
     * @param returnActive Parameter of type {@code Boolean} used by this operation.
     * @param returnPriceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param returnPriceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param returnStockMin Parameter of type {@code Integer} used by this operation.
     * @param returnStockMax Parameter of type {@code Integer} used by this operation.
     * @param returnSort Parameter of type {@code String} used by this operation.
     * @param returnDir Parameter of type {@code String} used by this operation.
     * @param returnPage Parameter of type {@code Integer} used by this operation.
     * @param returnSize Parameter of type {@code Integer} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping
    public String save(@ModelAttribute Product product,
                       @RequestParam(required = false) Long categoryId,
                       @RequestParam(required = false) MultipartFile imageFile,
                       @RequestParam(name = "returnCategoryId", required = false) Long returnCategoryId,
                       @RequestParam(name = "returnLowStock", required = false) Boolean returnLowStock,
                       @RequestParam(name = "returnQ", required = false) String returnQ,
                       @RequestParam(name = "returnActive", required = false) Boolean returnActive,
                       @RequestParam(name = "returnPriceMin", required = false) BigDecimal returnPriceMin,
                       @RequestParam(name = "returnPriceMax", required = false) BigDecimal returnPriceMax,
                       @RequestParam(name = "returnStockMin", required = false) Integer returnStockMin,
                       @RequestParam(name = "returnStockMax", required = false) Integer returnStockMax,
                       @RequestParam(name = "returnSort", required = false) String returnSort,
                       @RequestParam(name = "returnDir", required = false) String returnDir,
                       @RequestParam(name = "returnPage", required = false) Integer returnPage,
                       @RequestParam(name = "returnSize", required = false) Integer returnSize) {
        String listRedirect = buildListRedirect(returnCategoryId, returnLowStock, returnQ, returnActive, returnPriceMin,
                returnPriceMax, returnStockMin, returnStockMax, returnSort, returnDir, returnPage, returnSize);
        Product existing = null;
        int currentStock = 0;
        if (product.getId() != null) {
            existing = productRepo.findById(product.getId()).orElse(null);
            if (existing == null) {
                return "redirect:" + appendErrorCode(listRedirect, "notFound");
            }
            currentStock = existing.getStockQty() == null ? 0 : existing.getStockQty();
        }
        product.setStockQty(currentStock);

        normalizeEmptyStrings(product);
        normalizeNumbers(product);
        if (hasInvalidDateRange(product)) {
            return "redirect:" + appendErrorCode(listRedirect, "invalidDateRange");
        }
        if (categoryId != null) {
            product.setCategory(categoryRepo.findById(categoryId).orElse(null));
        } else {
            product.setCategory(null);
        }
        if (product.getActive() == null) {
            product.setActive(false);
        }
        if (product.getAllowNegativeStock() == null) {
            product.setAllowNegativeStock(false);
        }
        applyDeleteLifecycle(product);
        if (imageFile != null && !imageFile.isEmpty()) {
            if (imageFile.getContentType() == null || !imageFile.getContentType().startsWith("image/")) {
                return "redirect:" + appendErrorCode(listRedirect, "invalidImage");
            }
            String imageUrl = storeImage(imageFile);
            if (imageUrl == null) {
                return "redirect:" + appendErrorCode(listRedirect, "uploadFailed");
            }
            product.setImageUrl(imageUrl);
        } else if (product.getImageUrl() != null && product.getImageUrl().length() > 2048) {
            return "redirect:" + appendErrorCode(listRedirect, "imageUrlTooLong");
        }
        try {
            productRepo.save(product);
        } catch (DataIntegrityViolationException ex) {
            return "redirect:" + appendErrorCode(listRedirect, "duplicate");
        }
        return "redirect:" + listRedirect;
    }

    /**
     * Executes the importInventory operation.
     *
     * @param file Parameter of type {@code MultipartFile} used by this operation.
     * @param allowCreate Parameter of type {@code boolean} used by this operation.
     * @param createCategories Parameter of type {@code boolean} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the importInventory operation.
     *
     * @param file Parameter of type {@code MultipartFile} used by this operation.
     * @param allowCreate Parameter of type {@code boolean} used by this operation.
     * @param createCategories Parameter of type {@code boolean} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the importInventory operation.
     *
     * @param file Parameter of type {@code MultipartFile} used by this operation.
     * @param allowCreate Parameter of type {@code boolean} used by this operation.
     * @param createCategories Parameter of type {@code boolean} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/import")
    public String importInventory(@RequestParam("file") MultipartFile file,
                                  @RequestParam(required = false, defaultValue = "false") boolean allowCreate,
                                  @RequestParam(required = false, defaultValue = "false") boolean createCategories,
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
                                  @RequestParam(required = false) Integer size,
                                  RedirectAttributes redirectAttributes) {
        String listRedirect = buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                stockMin, stockMax, sort, dir, page, size);
        if (file == null || file.isEmpty()) {
            redirectAttributes.addFlashAttribute("error", "Please choose a CSV or Excel file to import.");
            return "redirect:" + listRedirect;
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
                return "redirect:" + listRedirect;
            }
        } catch (IOException ex) {
            redirectAttributes.addFlashAttribute("error", "Import failed: " + ex.getMessage());
            return "redirect:" + listRedirect;
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
        inventoryService.recordImportSummary(
                file.getOriginalFilename(),
                allowCreate,
                createCategories,
                result.created,
                result.updated,
                result.skipped,
                result.failed
        );
        redirectAttributes.addFlashAttribute("importSummary", summary);
        if (result.failed == 0) {
            redirectAttributes.addFlashAttribute("success", "Import completed. " + summary);
        } else {
            redirectAttributes.addFlashAttribute("error", "Import completed with some errors. " + summary);
        }
        return "redirect:" + listRedirect;
    }

    /**
     * Executes the exportCsv operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the exportCsv operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the exportCsv operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
            writer.println("ID,Name,SKU,Barcode,Category,Price,Cost,Stock,LowStockThreshold,Active,BasicUnit,BoxSpecifications,WeightValue,WeightUnit,LengthValue,LengthUnit,WidthValue,WidthUnit,HeightValue,HeightUnit,ManufactureDate,ExpirationDate,DeletedStatus,UpdatedAt,DeletedAt");
            for (Product product : products) {
                writer.println(buildCsvRow(product));
            }
        }
    }

    /**
     * Executes the exportExcel operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the exportExcel operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the exportExcel operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
                    "ID", "Name", "SKU", "Barcode", "Category", "Price", "Cost", "Stock", "Low Stock", "Active",
                    "Basic Unit", "Box Specifications",
                    "Weight Value", "Weight Unit",
                    "Length Value", "Length Unit",
                    "Width Value", "Width Unit",
                    "Height Value", "Height Unit",
                    "Manufacture Date", "Expiration Date",
                    "Deleted Status", "Updated At", "Deleted At"
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
                row.createCell(10).setCellValue(nullToEmpty(product.getBasicUnit()));
                row.createCell(11).setCellValue(nullToEmpty(product.getBoxSpecifications()));
                row.createCell(12).setCellValue(product.getWeightValue() == null ? "" : product.getWeightValue().toPlainString());
                row.createCell(13).setCellValue(nullToEmpty(product.getWeightUnit()));
                row.createCell(14).setCellValue(product.getLengthValue() == null ? "" : product.getLengthValue().toPlainString());
                row.createCell(15).setCellValue(nullToEmpty(product.getLengthUnit()));
                row.createCell(16).setCellValue(product.getWidthValue() == null ? "" : product.getWidthValue().toPlainString());
                row.createCell(17).setCellValue(nullToEmpty(product.getWidthUnit()));
                row.createCell(18).setCellValue(product.getHeightValue() == null ? "" : product.getHeightValue().toPlainString());
                row.createCell(19).setCellValue(nullToEmpty(product.getHeightUnit()));
                row.createCell(20).setCellValue(product.getManufactureDate() == null ? "" : product.getManufactureDate().toString());
                row.createCell(21).setCellValue(product.getExpirationDate() == null ? "" : product.getExpirationDate().toString());
                row.createCell(22).setCellValue(Boolean.TRUE.equals(product.getDeletedStatus()));
                row.createCell(23).setCellValue(product.getUpdatedAt() == null ? "" : product.getUpdatedAt().toString());
                row.createCell(24).setCellValue(product.getDeletedAt() == null ? "" : product.getDeletedAt().toString());
            }

            for (int i = 0; i < headers.length; i++) {
                sheet.autoSizeColumn(i);
            }

            workbook.write(response.getOutputStream());
        }
    }

    /**
     * Executes the quickUpdate operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param price Parameter of type {@code String} used by this operation.
     * @param stockQty Parameter of type {@code String} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the quickUpdate operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param price Parameter of type {@code String} used by this operation.
     * @param stockQty Parameter of type {@code String} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the quickUpdate operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param price Parameter of type {@code String} used by this operation.
     * @param stockQty Parameter of type {@code String} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
                              @RequestParam(required = false) Integer size,
                              RedirectAttributes redirectAttributes) {
        try {
            Product updated = inventoryService.quickUpdate(id, price, stockQty);
            redirectAttributes.addFlashAttribute("success", "Updated " + safeName(updated) + ".");
        } catch (RuntimeException ex) {
            redirectAttributes.addFlashAttribute("error", ex.getMessage());
        }
        return "redirect:" + buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                stockMin, stockMax, sort, dir, page, size);
    }

    /**
     * Executes the bulkStockAdjust operation.
     *
     * @param ids Parameter of type {@code List<Long>} used by this operation.
     * @param operation Parameter of type {@code String} used by this operation.
     * @param qty Parameter of type {@code String} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the bulkStockAdjust operation.
     *
     * @param ids Parameter of type {@code List<Long>} used by this operation.
     * @param operation Parameter of type {@code String} used by this operation.
     * @param qty Parameter of type {@code String} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the bulkStockAdjust operation.
     *
     * @param ids Parameter of type {@code List<Long>} used by this operation.
     * @param operation Parameter of type {@code String} used by this operation.
     * @param qty Parameter of type {@code String} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/bulk-stock")
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
                                  @RequestParam(required = false) Integer size,
                                  RedirectAttributes redirectAttributes) {
        try {
            int updated = inventoryService.bulkAdjustStock(ids, operation, qty);
            redirectAttributes.addFlashAttribute("success", "Adjusted stock for " + updated + " products.");
        } catch (RuntimeException ex) {
            redirectAttributes.addFlashAttribute("error", ex.getMessage());
        }
        return "redirect:" + buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                stockMin, stockMax, sort, dir, page, size);
    }

    /**
     * Executes the delete operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the delete operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the delete operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/{id}/delete")
    public String delete(@PathVariable Long id,
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
                         @RequestParam(required = false) Integer size) {
        Product product = productRepo.findById(id).orElse(null);
        if (product != null) {
            product.setDeletedStatus(true);
            product.setDeletedAt(LocalDateTime.now());
            product.setActive(false);
            // Release unique identifiers so new products can reuse old SKU/barcode values.
            product.setSku(null);
            product.setBarcode(null);
            productRepo.save(product);
        }
        return "redirect:" + buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                stockMin, stockMax, sort, dir, page, size);
    }

    /**
     * Executes the toggleActive operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the toggleActive operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the toggleActive operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
                               @RequestParam(required = false) Integer page,
                               @RequestParam(required = false) Integer size) {
        Product product = productRepo.findById(id).orElseThrow();
        boolean nextActive = !Boolean.TRUE.equals(product.getActive());
        product.setActive(nextActive);
        productRepo.save(product);
        return "redirect:" + buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                stockMin, stockMax, sort, dir, page, size);
    }

    /**
     * Executes the storeImage operation.
     *
     * @param imageFile Parameter of type {@code MultipartFile} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the normalizeEmptyStrings operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
        if (product.getBoxSpecifications() != null && product.getBoxSpecifications().trim().isEmpty()) {
            product.setBoxSpecifications(null);
        }
        if (product.getWeightUnit() != null && product.getWeightUnit().trim().isEmpty()) {
            product.setWeightUnit(null);
        }
        if (product.getLengthUnit() != null && product.getLengthUnit().trim().isEmpty()) {
            product.setLengthUnit(null);
        }
        if (product.getWidthUnit() != null && product.getWidthUnit().trim().isEmpty()) {
            product.setWidthUnit(null);
        }
        if (product.getHeightUnit() != null && product.getHeightUnit().trim().isEmpty()) {
            product.setHeightUnit(null);
        }
        if (product.getBasicUnit() != null && product.getBasicUnit().trim().isEmpty()) {
            product.setBasicUnit(null);
        }
    }

    /**
     * Executes the normalizeNumbers operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
        if (product.getWeightValue() != null && product.getWeightValue().compareTo(BigDecimal.ZERO) <= 0) {
            product.setWeightValue(null);
        }
        if (product.getLengthValue() != null && product.getLengthValue().compareTo(BigDecimal.ZERO) <= 0) {
            product.setLengthValue(null);
        }
        if (product.getWidthValue() != null && product.getWidthValue().compareTo(BigDecimal.ZERO) <= 0) {
            product.setWidthValue(null);
        }
        if (product.getHeightValue() != null && product.getHeightValue().compareTo(BigDecimal.ZERO) <= 0) {
            product.setHeightValue(null);
        }
    }

    /**
     * Executes the hasInvalidDateRange operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean hasInvalidDateRange(Product product) {
        LocalDate manufactureDate = product.getManufactureDate();
        LocalDate expirationDate = product.getExpirationDate();
        return manufactureDate != null && expirationDate != null && expirationDate.isBefore(manufactureDate);
    }

    /**
     * Executes the applyDeleteLifecycle operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void applyDeleteLifecycle(Product product) {
        if (product.getDeletedStatus() == null) {
            product.setDeletedStatus(false);
        }
        if (Boolean.TRUE.equals(product.getDeletedStatus())) {
            if (product.getDeletedAt() == null) {
                product.setDeletedAt(LocalDateTime.now());
            }
            product.setActive(false);
        } else {
            product.setDeletedAt(null);
        }
    }

    /**
     * Executes the buildSpecification operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param onlyLowStock Parameter of type {@code boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @return {@code Specification<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
            var deletedStatusExpr = root.get("deletedStatus").as(Boolean.class);
            predicates.add(cb.or(
                    cb.isNull(deletedStatusExpr),
                    cb.isFalse(deletedStatusExpr)
            ));
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

    /**
     * Executes the buildSort operation.
     *
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @return {@code Sort} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the findFilteredProducts operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @return {@code List<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the buildCsvRow operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
                csv(Boolean.TRUE.equals(product.getActive()) ? "Active" : "Inactive"),
                csv(nullToEmpty(product.getBasicUnit())),
                csv(nullToEmpty(product.getBoxSpecifications())),
                csv(product.getWeightValue() == null ? "" : product.getWeightValue().toPlainString()),
                csv(nullToEmpty(product.getWeightUnit())),
                csv(product.getLengthValue() == null ? "" : product.getLengthValue().toPlainString()),
                csv(nullToEmpty(product.getLengthUnit())),
                csv(product.getWidthValue() == null ? "" : product.getWidthValue().toPlainString()),
                csv(nullToEmpty(product.getWidthUnit())),
                csv(product.getHeightValue() == null ? "" : product.getHeightValue().toPlainString()),
                csv(nullToEmpty(product.getHeightUnit())),
                csv(product.getManufactureDate() == null ? "" : product.getManufactureDate().toString()),
                csv(product.getExpirationDate() == null ? "" : product.getExpirationDate().toString()),
                csv(Boolean.TRUE.equals(product.getDeletedStatus()) ? "true" : "false"),
                csv(product.getUpdatedAt() == null ? "" : product.getUpdatedAt().toString()),
                csv(product.getDeletedAt() == null ? "" : product.getDeletedAt().toString())
        );
    }

    /**
     * Executes the csv operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String csv(String value) {
        if (value == null) return "";
        String escaped = value.replace("\"", "\"\"");
        if (escaped.contains(",") || escaped.contains("\"") || escaped.contains("\n")) {
            return "\"" + escaped + "\"";
        }
        return escaped;
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
     * Executes the nullToEmpty operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String nullToEmpty(String value) {
        return value == null ? "" : value;
    }

    /**
     * Executes the importFromCsv operation.
     *
     * @param file Parameter of type {@code MultipartFile} used by this operation.
     * @param allowCreate Parameter of type {@code boolean} used by this operation.
     * @param createCategories Parameter of type {@code boolean} used by this operation.
     * @return {@code ImportResult} Result produced by this operation.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the importFromExcel operation.
     *
     * @param file Parameter of type {@code MultipartFile} used by this operation.
     * @param allowCreate Parameter of type {@code boolean} used by this operation.
     * @param createCategories Parameter of type {@code boolean} used by this operation.
     * @return {@code ImportResult} Result produced by this operation.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the applyImportRow operation.
     *
     * @param row Parameter of type {@code Map<String, String>} used by this operation.
     * @param allowCreate Parameter of type {@code boolean} used by this operation.
     * @param createCategories Parameter of type {@code boolean} used by this operation.
     * @param errors Parameter of type {@code List<String>} used by this operation.
     * @param rowNum Parameter of type {@code int} used by this operation.
     * @return {@code ImportOutcome} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

        Integer targetStock = null;
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
            targetStock = stockQty;
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
        String boxSpecificationsRaw = trimToNull(row.get("boxSpecifications"));
        if (boxSpecificationsRaw != null) {
            product.setBoxSpecifications(boxSpecificationsRaw);
        }

        String basicUnitRaw = trimToNull(row.get("basicUnit"));
        if (basicUnitRaw != null) {
            product.setBasicUnit(basicUnitRaw);
        }

        String weightRaw = row.get("weightValue");
        if (hasText(weightRaw)) {
            BigDecimal weightValue = parseBigDecimal(weightRaw);
            if (weightValue == null) {
                errors.add("Row " + rowNum + ": invalid weight value.");
                return ImportOutcome.FAILED;
            }
            product.setWeightValue(weightValue);
        }
        String weightUnitRaw = trimToNull(row.get("weightUnit"));
        if (weightUnitRaw != null) {
            product.setWeightUnit(weightUnitRaw);
        }

        String lengthRaw = row.get("lengthValue");
        if (hasText(lengthRaw)) {
            BigDecimal lengthValue = parseBigDecimal(lengthRaw);
            if (lengthValue == null) {
                errors.add("Row " + rowNum + ": invalid length value.");
                return ImportOutcome.FAILED;
            }
            product.setLengthValue(lengthValue);
        }
        String lengthUnitRaw = trimToNull(row.get("lengthUnit"));
        if (lengthUnitRaw != null) {
            product.setLengthUnit(lengthUnitRaw);
        }

        String widthRaw = row.get("widthValue");
        if (hasText(widthRaw)) {
            BigDecimal widthValue = parseBigDecimal(widthRaw);
            if (widthValue == null) {
                errors.add("Row " + rowNum + ": invalid width value.");
                return ImportOutcome.FAILED;
            }
            product.setWidthValue(widthValue);
        }
        String widthUnitRaw = trimToNull(row.get("widthUnit"));
        if (widthUnitRaw != null) {
            product.setWidthUnit(widthUnitRaw);
        }

        String heightRaw = row.get("heightValue");
        if (hasText(heightRaw)) {
            BigDecimal heightValue = parseBigDecimal(heightRaw);
            if (heightValue == null) {
                errors.add("Row " + rowNum + ": invalid height value.");
                return ImportOutcome.FAILED;
            }
            product.setHeightValue(heightValue);
        }
        String heightUnitRaw = trimToNull(row.get("heightUnit"));
        if (heightUnitRaw != null) {
            product.setHeightUnit(heightUnitRaw);
        }

        String manufactureDateRaw = row.get("manufactureDate");
        if (hasText(manufactureDateRaw)) {
            LocalDate manufactureDate = parseLocalDate(manufactureDateRaw);
            if (manufactureDate == null) {
                errors.add("Row " + rowNum + ": invalid manufacture date.");
                return ImportOutcome.FAILED;
            }
            product.setManufactureDate(manufactureDate);
        }
        String expirationDateRaw = row.get("expirationDate");
        if (hasText(expirationDateRaw)) {
            LocalDate expirationDate = parseLocalDate(expirationDateRaw);
            if (expirationDate == null) {
                errors.add("Row " + rowNum + ": invalid expiration date.");
                return ImportOutcome.FAILED;
            }
            product.setExpirationDate(expirationDate);
        }

        String deletedStatusRaw = row.get("deletedStatus");
        if (hasText(deletedStatusRaw)) {
            Boolean deletedStatus = parseBoolean(deletedStatusRaw);
            if (deletedStatus == null) {
                errors.add("Row " + rowNum + ": invalid deleted status value.");
                return ImportOutcome.FAILED;
            }
            product.setDeletedStatus(deletedStatus);
        }
        String deletedAtRaw = row.get("deletedAt");
        if (hasText(deletedAtRaw)) {
            LocalDateTime deletedAt = parseLocalDateTime(deletedAtRaw);
            if (deletedAt == null) {
                errors.add("Row " + rowNum + ": invalid deleted date.");
                return ImportOutcome.FAILED;
            }
            product.setDeletedAt(deletedAt);
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

        if (product.getId() == null && product.getStockQty() == null) {
            product.setStockQty(0);
        }
        normalizeEmptyStrings(product);
        normalizeNumbers(product);
        if (hasInvalidDateRange(product)) {
            errors.add("Row " + rowNum + ": expiration date must be on or after manufacture date.");
            return ImportOutcome.FAILED;
        }
        applyDeleteLifecycle(product);

        try {
            Product saved = productRepo.save(product);
            if (targetStock != null) {
                inventoryService.setStockFromImport(
                        saved.getId(),
                        targetStock,
                        "IMPORT_ROW:" + rowNum,
                        "Inventory import"
                );
            }
        } catch (DataIntegrityViolationException ex) {
            errors.add("Row " + rowNum + ": duplicate SKU or barcode.");
            return ImportOutcome.FAILED;
        } catch (IllegalStateException ex) {
            errors.add("Row " + rowNum + ": " + ex.getMessage());
            return ImportOutcome.FAILED;
        }

        return created ? ImportOutcome.CREATED : ImportOutcome.UPDATED;
    }

    /**
     * Executes the buildHeaderIndex operation.
     *
     * @param headers Parameter of type {@code List<String>} used by this operation.
     * @return {@code Map<String, Integer>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the mapRow operation.
     *
     * @param values Parameter of type {@code List<String>} used by this operation.
     * @param headerIndex Parameter of type {@code Map<String, Integer>} used by this operation.
     * @return {@code Map<String, String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, String> mapRow(List<String> values, Map<String, Integer> headerIndex) {
        Map<String, String> row = new HashMap<>();
        for (Map.Entry<String, Integer> entry : headerIndex.entrySet()) {
            int idx = entry.getValue();
            row.put(entry.getKey(), idx < values.size() ? values.get(idx) : "");
        }
        return row;
    }

    /**
     * Executes the normalizeHeader operation.
     *
     * @param header Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalizeHeader(String header) {
        if (header == null) return "";
        return header.trim().toLowerCase().replaceAll("[^a-z0-9]", "");
    }

    /**
     * Executes the mapHeader operation.
     *
     * @param normalized Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
            case "boxspecifications", "boxspec" -> "boxSpecifications";
            case "basicunit", "baseunit" -> "basicUnit";
            case "weight", "weightvalue" -> "weightValue";
            case "weightunit" -> "weightUnit";
            case "length", "lengthvalue" -> "lengthValue";
            case "lengthunit" -> "lengthUnit";
            case "width", "widthvalue" -> "widthValue";
            case "widthunit" -> "widthUnit";
            case "height", "heightvalue" -> "heightValue";
            case "heightunit" -> "heightUnit";
            case "manufacturedate", "manufacturingdate", "mfgdate" -> "manufactureDate";
            case "expirationdate", "expirydate", "expdate" -> "expirationDate";
            case "deletedstatus", "isdeleted" -> "deletedStatus";
            case "deletedat", "deleteddate" -> "deletedAt";
            default -> null;
        };
    }

    /**
     * Executes the parseCsvLine operation.
     *
     * @param line Parameter of type {@code String} used by this operation.
     * @return {@code List<String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the parseInteger operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code Integer} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Integer parseInteger(String value) {
        if (!hasText(value)) return null;
        try {
            return Integer.parseInt(value.trim().replace(",", ""));
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    /**
     * Executes the parseLong operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code Long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Long parseLong(String value) {
        if (!hasText(value)) return null;
        try {
            return Long.parseLong(value.trim().replace(",", ""));
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    /**
     * Executes the parseBigDecimal operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal parseBigDecimal(String value) {
        if (!hasText(value)) return null;
        try {
            return new BigDecimal(value.trim().replace(",", ""));
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    /**
     * Executes the parseBoolean operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code Boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Boolean parseBoolean(String value) {
        if (!hasText(value)) return null;
        String normalized = value.trim().toLowerCase();
        if (normalized.equals("true") || normalized.equals("yes") || normalized.equals("1")
                || normalized.equals("active") || normalized.equals("deleted")) {
            return true;
        }
        if (normalized.equals("false") || normalized.equals("no") || normalized.equals("0")
                || normalized.equals("inactive") || normalized.equals("notdeleted")) {
            return false;
        }
        return null;
    }

    /**
     * Executes the parseLocalDate operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code LocalDate} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private LocalDate parseLocalDate(String value) {
        if (!hasText(value)) return null;
        String normalized = value.trim();
        try {
            return LocalDate.parse(normalized);
        } catch (DateTimeParseException ignored) {
            // Support common imports where date-time is provided for a date-only field.
            LocalDateTime dateTime = parseLocalDateTime(normalized);
            return dateTime == null ? null : dateTime.toLocalDate();
        }
    }

    /**
     * Executes the parseLocalDateTime operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code LocalDateTime} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private LocalDateTime parseLocalDateTime(String value) {
        if (!hasText(value)) return null;
        String normalized = value.trim().replace(" ", "T");
        try {
            return LocalDateTime.parse(normalized);
        } catch (DateTimeParseException ignored) {
            try {
                return LocalDate.parse(normalized).atStartOfDay();
            } catch (DateTimeParseException ignoredAgain) {
                return null;
            }
        }
    }

    /**
     * Executes the trimToNull operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

        /**
         * Executes the increment operation.
         *
         * @param outcome Parameter of type {@code ImportOutcome} used by this operation.
         * @return void No value is returned; the method applies side effects to existing state.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
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

    /**
     * Executes the buildProductListStats operation.
     *
     * @param products Parameter of type {@code List<Product>} used by this operation.
     * @param categories Parameter of type {@code List<Category>} used by this operation.
     * @return {@code ProductListStats} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the topStockValue operation.
     *
     * @param products Parameter of type {@code List<Product>} used by this operation.
     * @param limit Parameter of type {@code int} used by this operation.
     * @return {@code List<ProductValue>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the topMargin operation.
     *
     * @param products Parameter of type {@code List<Product>} used by this operation.
     * @param limit Parameter of type {@code int} used by this operation.
     * @return {@code List<ProductMargin>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the topCategoryValue operation.
     *
     * @param products Parameter of type {@code List<Product>} used by this operation.
     * @param categoryNames Parameter of type {@code Map<Long, String>} used by this operation.
     * @param limit Parameter of type {@code int} used by this operation.
     * @return {@code List<CategoryMetric>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the addTop operation.
     *
     * @param queue Parameter of type {@code PriorityQueue<T>} used by this operation.
     * @param value Parameter of type {@code T} used by this operation.
     * @param limit Parameter of type {@code int} used by this operation.
     * @param comparator Parameter of type {@code Comparator<T>} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the safeName operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String safeName(Product product) {
        if (product == null) return "-";
        if (hasText(product.getName())) return product.getName();
        if (hasText(product.getSku())) return product.getSku();
        return "Product";
    }

    /**
     * Executes the hasText operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean hasText(String value) {
        return value != null && !value.trim().isEmpty();
    }

    /**
     * Executes the addProductAnalytics operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @param product Parameter of type {@code Product} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

        /**
         * Executes the CategoryMetricBuilder operation.
         * <p>Return value: A fully initialized CategoryMetricBuilder instance.</p>
         *
         * @param name Parameter of type {@code String} used by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
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

    // Keep current list state in the form so save/cancel can return users to the same page.
    /**
     * Executes the addReturnState operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void addReturnState(Model model,
                                Long categoryId,
                                Boolean lowStock,
                                String q,
                                Boolean active,
                                BigDecimal priceMin,
                                BigDecimal priceMax,
                                Integer stockMin,
                                Integer stockMax,
                                String sort,
                                String dir,
                                Integer page,
                                Integer size) {
        int safePage = page == null ? 0 : Math.max(0, page);
        int safeSize = normalizePageSize(size);
        model.addAttribute("returnCategoryId", categoryId);
        model.addAttribute("returnLowStock", Boolean.TRUE.equals(lowStock));
        model.addAttribute("returnQ", q);
        model.addAttribute("returnActive", active);
        model.addAttribute("returnPriceMin", priceMin);
        model.addAttribute("returnPriceMax", priceMax);
        model.addAttribute("returnStockMin", stockMin);
        model.addAttribute("returnStockMax", stockMax);
        model.addAttribute("returnSort", sort);
        model.addAttribute("returnDir", dir);
        model.addAttribute("returnPage", safePage);
        model.addAttribute("returnSize", safeSize);
        model.addAttribute("returnToListUrl", buildListRedirect(categoryId, lowStock, q, active, priceMin, priceMax,
                stockMin, stockMax, sort, dir, safePage, safeSize));
    }

    /**
     * Executes the appendErrorCode operation.
     *
     * @param baseRedirect Parameter of type {@code String} used by this operation.
     * @param errorCode Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String appendErrorCode(String baseRedirect, String errorCode) {
        if (!hasText(errorCode)) {
            return baseRedirect;
        }
        String separator = baseRedirect.contains("?") ? "&" : "?";
        return baseRedirect + separator + "error=" + java.net.URLEncoder.encode(errorCode, java.nio.charset.StandardCharsets.UTF_8);
    }

    /**
     * Executes the normalizePageSize operation.
     *
     * @param requestedSize Parameter of type {@code Integer} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int normalizePageSize(Integer requestedSize) {
        if (requestedSize == null || requestedSize <= 0) {
            return DEFAULT_PAGE_SIZE;
        }
        return Math.min(requestedSize, MAX_PAGE_SIZE);
    }

    // Rebuild the list URL with paging/sorting/filter state after item-level actions.
    /**
     * Executes the buildListRedirect operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param lowStock Parameter of type {@code Boolean} used by this operation.
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param priceMin Parameter of type {@code BigDecimal} used by this operation.
     * @param priceMax Parameter of type {@code BigDecimal} used by this operation.
     * @param stockMin Parameter of type {@code Integer} used by this operation.
     * @param stockMax Parameter of type {@code Integer} used by this operation.
     * @param sort Parameter of type {@code String} used by this operation.
     * @param dir Parameter of type {@code String} used by this operation.
     * @param page Parameter of type {@code Integer} used by this operation.
     * @param size Parameter of type {@code Integer} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String buildListRedirect(Long categoryId, Boolean lowStock, String q, Boolean active,
                                     BigDecimal priceMin, BigDecimal priceMax,
                                     Integer stockMin, Integer stockMax,
                                     String sort, String dir, Integer page, Integer size) {
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
        if (page != null && page >= 0) {
            redirect.append(sep).append("page=").append(page);
            sep = "&";
        }
        if (size != null) {
            redirect.append(sep).append("size=").append(normalizePageSize(size));
        }
        return redirect.toString();
    }
}
