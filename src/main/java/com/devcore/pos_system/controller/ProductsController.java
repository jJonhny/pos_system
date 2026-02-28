package com.devcore.pos_system.controller;

import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.Category;
import com.devcore.pos_system.entity.ProductUnit;
import com.devcore.pos_system.repository.CategoryRepo;
import com.devcore.pos_system.repository.ProductRepo;
import com.devcore.pos_system.service.InventoryService;
import com.devcore.pos_system.service.MasterStockUnitService;
import com.devcore.pos_system.service.ProductUnitAdminService;
import com.devcore.pos_system.service.ProductUnitAdminService.ProductUnitDraft;
import com.devcore.pos_system.service.ProductUnitConversionService;

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
import javax.imageio.ImageIO;

import java.io.BufferedReader;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.awt.image.BufferedImage;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeParseException;
import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Locale;
import java.util.PriorityQueue;
import java.util.UUID;
import java.util.Collections;
import java.util.stream.Collectors;
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
    private static final long MAX_IMAGE_SIZE_BYTES = 10L * 1024L * 1024L;
    private static final int MAX_IMAGE_DIMENSION = 1600;
    private static final int THUMBNAIL_IMAGE_DIMENSION = 320;
    private final ProductRepo productRepo;
    private final CategoryRepo categoryRepo;
    private final InventoryService inventoryService;
    private final MasterStockUnitService masterStockUnitService;
    private final ProductUnitAdminService productUnitAdminService;
    private final ProductUnitConversionService productUnitConversionService;

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
    public ProductsController(ProductRepo productRepo,
                              CategoryRepo categoryRepo,
                              InventoryService inventoryService,
                              MasterStockUnitService masterStockUnitService,
                              ProductUnitAdminService productUnitAdminService,
                              ProductUnitConversionService productUnitConversionService) {
        this.productRepo = productRepo;
        this.categoryRepo = categoryRepo;
        this.inventoryService = inventoryService;
        this.masterStockUnitService = masterStockUnitService;
        this.productUnitAdminService = productUnitAdminService;
        this.productUnitConversionService = productUnitConversionService;
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
        if ("invalidPricing".equals(error)) {
            model.addAttribute("error", "Pricing or threshold values are invalid. Check retail/wholesale/cost rules and quantity thresholds.");
        }
        if ("invalidUom".equals(error)) {
            model.addAttribute("error", "Packaging units are invalid. Check unit name, conversion, defaults, and barcode uniqueness.");
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
        product.setBaseUnitName("piece");
        product.setBaseUnitPrecision(0);
        model.addAttribute("product", product);
        model.addAttribute("categories", categoryRepo.findAll(Sort.by("sortOrder").ascending().and(Sort.by("name").ascending())));
        addUomFormModel(model, product);
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
        addUomFormModel(model, product);
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
                       @RequestParam(required = false, name = "retailPriceUnitToken") String retailPriceUnitToken,
                       @RequestParam(required = false, name = "wholesalePriceUnitToken") String wholesalePriceUnitToken,
                       @RequestParam(required = false, name = "wholesaleMinQtyUnitToken") String wholesaleMinQtyUnitToken,
                       @RequestParam(required = false, name = "lowStockThresholdUnitToken") String lowStockThresholdUnitToken,
                       @RequestParam(required = false, name = "packagingId") List<Long> packagingId,
                       @RequestParam(required = false, name = "packagingName") List<String> packagingName,
                       @RequestParam(required = false, name = "packagingAbbreviation") List<String> packagingAbbreviation,
                       @RequestParam(required = false, name = "packagingConversionToBase") List<BigDecimal> packagingConversionToBase,
                       @RequestParam(required = false, name = "packagingAllowForSale") List<String> packagingAllowForSale,
                       @RequestParam(required = false, name = "packagingAllowForPurchase") List<String> packagingAllowForPurchase,
                       @RequestParam(required = false, name = "packagingBarcode") List<String> packagingBarcode,
                       @RequestParam(required = false, name = "packagingDefaultSaleIndex") Integer packagingDefaultSaleIndex,
                       @RequestParam(required = false, name = "packagingDefaultPurchaseIndex") Integer packagingDefaultPurchaseIndex,
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

        List<ProductUnitDraft> unitDrafts = buildProductUnitDrafts(
                packagingId,
                packagingName,
                packagingAbbreviation,
                packagingConversionToBase,
                packagingAllowForSale,
                packagingAllowForPurchase,
                packagingBarcode
        );
        Map<Long, BigDecimal> existingConversions = existing == null
                ? Map.of()
                : productUnitAdminService.conversionMap(existing.getId());
        Map<String, BigDecimal> tokenConversionMap = buildTokenConversionMap(product, existing, existingConversions, unitDrafts);

        normalizeEmptyStrings(product);
        normalizeNumbers(product);
        try {
            validatePricingRules(product);
        } catch (IllegalArgumentException ex) {
            return "redirect:" + appendErrorCode(listRedirect, "invalidPricing");
        }

        UnitSelection retailSelection = resolveUnitSelection(retailPriceUnitToken, tokenConversionMap);
        UnitSelection wholesaleSelection = resolveUnitSelection(wholesalePriceUnitToken, tokenConversionMap);
        UnitSelection wholesaleMinSelection = resolveUnitSelection(wholesaleMinQtyUnitToken, tokenConversionMap);
        UnitSelection lowStockSelection = resolveUnitSelection(lowStockThresholdUnitToken, tokenConversionMap);

        try {
            product.setPrice(toBaseMoney(product.getPrice(), retailSelection.conversionToBase()));
            product.setWholesalePrice(toBaseMoney(product.getWholesalePrice(), wholesaleSelection.conversionToBase()));
            product.setCostPrice(scaleMoney(product.getCostPrice()));
            product.setWholesaleMinQty(toBaseQty(product.getWholesaleMinQty(), wholesaleMinSelection.conversionToBase()));
            product.setLowStockThreshold(toBaseQty(product.getLowStockThreshold(), lowStockSelection.conversionToBase()));
        } catch (IllegalArgumentException ex) {
            return "redirect:" + appendErrorCode(listRedirect, "invalidPricing");
        }
        if (product.getWholesalePrice() != null
                && (product.getWholesaleMinQty() == null || product.getWholesaleMinQty() < 1)) {
            return "redirect:" + appendErrorCode(listRedirect, "invalidPricing");
        }
        product.setRetailPriceUnitId(retailSelection.unitId());
        product.setWholesalePriceUnitId(wholesaleSelection.unitId());
        product.setWholesaleMinQtyUnitId(wholesaleMinSelection.unitId());
        product.setLowStockThresholdUnitId(lowStockSelection.unitId());

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
        if (!Boolean.TRUE.equals(product.getAllowNegativeStock()) && currentStock < 0) {
            return "redirect:" + appendErrorCode(listRedirect, "invalidStock");
        }
        applyDeleteLifecycle(product);
        if (imageFile != null && !imageFile.isEmpty()) {
            if (imageFile.getSize() > MAX_IMAGE_SIZE_BYTES) {
                return "redirect:" + appendErrorCode(listRedirect, "uploadTooLarge");
            }
            if (!isAllowedImageContentType(imageFile.getContentType())) {
                return "redirect:" + appendErrorCode(listRedirect, "invalidImage");
            }
            BufferedImage decodedImage;
            try {
                decodedImage = ImageIO.read(imageFile.getInputStream());
            } catch (IOException ex) {
                decodedImage = null;
            }
            if (decodedImage == null || decodedImage.getWidth() <= 0 || decodedImage.getHeight() <= 0) {
                return "redirect:" + appendErrorCode(listRedirect, "invalidImage");
            }
            String imageUrl = storeImage(imageFile, decodedImage);
            if (imageUrl == null) {
                return "redirect:" + appendErrorCode(listRedirect, "uploadFailed");
            }
            product.setImageUrl(imageUrl);
        } else if (product.getImageUrl() != null && product.getImageUrl().length() > 2048) {
            return "redirect:" + appendErrorCode(listRedirect, "imageUrlTooLong");
        }
        try {
            Product saved = productRepo.save(product);
            List<ProductUnit> syncedUnits = productUnitAdminService.replaceUnits(
                    saved,
                    unitDrafts,
                    packagingDefaultSaleIndex,
                    packagingDefaultPurchaseIndex
            );
            productUnitAdminService.applyLegacyFallbackFields(saved, syncedUnits);
            saved.setRetailPriceUnitId(validateUnitOwnership(saved.getId(), saved.getRetailPriceUnitId()));
            saved.setWholesalePriceUnitId(validateUnitOwnership(saved.getId(), saved.getWholesalePriceUnitId()));
            saved.setWholesaleMinQtyUnitId(validateUnitOwnership(saved.getId(), saved.getWholesaleMinQtyUnitId()));
            saved.setLowStockThresholdUnitId(validateUnitOwnership(saved.getId(), saved.getLowStockThresholdUnitId()));
            productRepo.save(saved);
        } catch (ProductUnitAdminService.ProductUnitValidationException ex) {
            return "redirect:" + appendErrorCode(listRedirect, "invalidUom");
        } catch (DataIntegrityViolationException ex) {
            return "redirect:" + appendErrorCode(listRedirect, "duplicate");
        } catch (IllegalArgumentException ex) {
            return "redirect:" + appendErrorCode(listRedirect, "invalidPricing");
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
            writer.println("ID,Name,SKU,Barcode,Category,Price,Cost,Stock,LowStockThreshold,Active,BasicUnit,BaseUnitName,BaseUnitPrecision,BoxSpecifications,WeightValue,WeightUnit,LengthValue,LengthUnit,WidthValue,WidthUnit,HeightValue,HeightUnit,ManufactureDate,ExpirationDate,DeletedStatus,UpdatedAt,DeletedAt");
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
                    "Basic Unit", "Base Unit Name", "Base Unit Precision", "Box Specifications",
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
                row.createCell(11).setCellValue(nullToEmpty(product.getBaseUnitName()));
                row.createCell(12).setCellValue(product.getBaseUnitPrecision() == null ? "" : String.valueOf(product.getBaseUnitPrecision()));
                row.createCell(13).setCellValue(nullToEmpty(product.getBoxSpecifications()));
                row.createCell(14).setCellValue(product.getWeightValue() == null ? "" : product.getWeightValue().toPlainString());
                row.createCell(15).setCellValue(nullToEmpty(product.getWeightUnit()));
                row.createCell(16).setCellValue(product.getLengthValue() == null ? "" : product.getLengthValue().toPlainString());
                row.createCell(17).setCellValue(nullToEmpty(product.getLengthUnit()));
                row.createCell(18).setCellValue(product.getWidthValue() == null ? "" : product.getWidthValue().toPlainString());
                row.createCell(19).setCellValue(nullToEmpty(product.getWidthUnit()));
                row.createCell(20).setCellValue(product.getHeightValue() == null ? "" : product.getHeightValue().toPlainString());
                row.createCell(21).setCellValue(nullToEmpty(product.getHeightUnit()));
                row.createCell(22).setCellValue(product.getManufactureDate() == null ? "" : product.getManufactureDate().toString());
                row.createCell(23).setCellValue(product.getExpirationDate() == null ? "" : product.getExpirationDate().toString());
                row.createCell(24).setCellValue(Boolean.TRUE.equals(product.getDeletedStatus()));
                row.createCell(25).setCellValue(product.getUpdatedAt() == null ? "" : product.getUpdatedAt().toString());
                row.createCell(26).setCellValue(product.getDeletedAt() == null ? "" : product.getDeletedAt().toString());
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
    private String storeImage(MultipartFile imageFile, BufferedImage decodedImage) {
        String filename = UUID.randomUUID().toString().replace("-", "") + ".jpg";

        Path uploadDir = Paths.get("src/main/resources/static/uploads");
        Path thumbDir = uploadDir.resolve("thumbs");
        Path targetDir = Paths.get("target/classes/static/uploads");
        Path targetThumbDir = targetDir.resolve("thumbs");
        try {
            Files.createDirectories(uploadDir);
            Files.createDirectories(thumbDir);
            Path saved = uploadDir.resolve(filename);
            BufferedImage normalized = ensureRgb(resizeToFit(decodedImage, MAX_IMAGE_DIMENSION));
            BufferedImage thumbnail = ensureRgb(resizeToFit(decodedImage, THUMBNAIL_IMAGE_DIMENSION));
            if (!ImageIO.write(normalized, "jpg", saved.toFile())) {
                return null;
            }
            Path thumbFile = thumbDir.resolve(filename);
            if (!ImageIO.write(thumbnail, "jpg", thumbFile.toFile())) {
                return null;
            }
            try {
                Files.createDirectories(targetDir);
                Files.createDirectories(targetThumbDir);
                Files.copy(saved, targetDir.resolve(filename), StandardCopyOption.REPLACE_EXISTING);
                Files.copy(thumbFile, targetThumbDir.resolve(filename), StandardCopyOption.REPLACE_EXISTING);
            } catch (IOException ignored) {
                // best effort: keep dev-time live classpath folder updated
            }
        } catch (IOException e) {
            return null;
        }
        return "/uploads/" + filename;
    }

    private BufferedImage resizeToFit(BufferedImage source, int maxDimension) {
        if (source == null) return null;
        int width = Math.max(1, source.getWidth());
        int height = Math.max(1, source.getHeight());
        if (maxDimension <= 0 || (width <= maxDimension && height <= maxDimension)) {
            return source;
        }
        double scale = Math.min((double) maxDimension / width, (double) maxDimension / height);
        int targetWidth = Math.max(1, (int) Math.round(width * scale));
        int targetHeight = Math.max(1, (int) Math.round(height * scale));

        BufferedImage resized = new BufferedImage(targetWidth, targetHeight, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = resized.createGraphics();
        try {
            graphics.setColor(Color.WHITE);
            graphics.fillRect(0, 0, targetWidth, targetHeight);
            graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
            graphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            graphics.drawImage(source, 0, 0, targetWidth, targetHeight, null);
        } finally {
            graphics.dispose();
        }
        return resized;
    }

    private BufferedImage ensureRgb(BufferedImage source) {
        if (source == null) return null;
        if (source.getType() == BufferedImage.TYPE_INT_RGB) {
            return source;
        }
        BufferedImage rgb = new BufferedImage(source.getWidth(), source.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = rgb.createGraphics();
        try {
            graphics.setColor(Color.WHITE);
            graphics.fillRect(0, 0, source.getWidth(), source.getHeight());
            graphics.drawImage(source, 0, 0, null);
        } finally {
            graphics.dispose();
        }
        return rgb;
    }

    private boolean isAllowedImageContentType(String contentType) {
        if (contentType == null) return false;
        String normalized = contentType.trim().toLowerCase(Locale.ROOT);
        return normalized.equals("image/jpeg")
                || normalized.equals("image/jpg")
                || normalized.equals("image/png")
                || normalized.equals("image/gif")
                || normalized.equals("image/webp");
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
        if (product.getBaseUnitName() != null && product.getBaseUnitName().trim().isEmpty()) {
            product.setBaseUnitName(null);
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
        if (product.getBaseUnitPrecision() != null && product.getBaseUnitPrecision() < 0) {
            product.setBaseUnitPrecision(0);
        }
    }

    private List<ProductUnitDraft> buildProductUnitDrafts(List<Long> ids,
                                                          List<String> names,
                                                          List<String> abbreviations,
                                                          List<BigDecimal> conversions,
                                                          List<String> allowForSale,
                                                          List<String> allowForPurchase,
                                                          List<String> barcodes) {
        int size = maxListSize(ids, names, abbreviations, conversions, allowForSale, allowForPurchase, barcodes);
        List<ProductUnitDraft> drafts = new ArrayList<>();
        for (int i = 0; i < size; i++) {
            Long id = at(ids, i);
            String name = at(names, i);
            String abbreviation = at(abbreviations, i);
            BigDecimal conversion = at(conversions, i);
            String saleFlag = at(allowForSale, i);
            String purchaseFlag = at(allowForPurchase, i);
            String barcode = at(barcodes, i);
            boolean hasAnyValue = id != null
                    || hasText(name)
                    || hasText(abbreviation)
                    || conversion != null
                    || hasText(saleFlag)
                    || hasText(purchaseFlag)
                    || hasText(barcode);
            if (!hasAnyValue) {
                continue;
            }
            drafts.add(new ProductUnitDraft(
                    id,
                    name,
                    abbreviation,
                    conversion,
                    parseBooleanValue(saleFlag),
                    parseBooleanValue(purchaseFlag),
                    barcode
            ));
        }
        return drafts;
    }

    private Map<String, BigDecimal> buildTokenConversionMap(Product incoming,
                                                            Product existing,
                                                            Map<Long, BigDecimal> existingConversions,
                                                            List<ProductUnitDraft> drafts) {
        Map<String, BigDecimal> map = new HashMap<>();
        map.put("BASE", BigDecimal.ONE);

        if (existingConversions != null) {
            for (Map.Entry<Long, BigDecimal> entry : existingConversions.entrySet()) {
                Long id = entry.getKey();
                BigDecimal conversion = entry.getValue();
                if (id == null || conversion == null || conversion.compareTo(BigDecimal.ZERO) <= 0) continue;
                map.put("UNIT:" + id, conversion);
            }
        }

        if (drafts != null) {
            for (ProductUnitDraft draft : drafts) {
                if (draft == null || draft.id() == null || draft.conversionToBase() == null) continue;
                if (draft.conversionToBase().compareTo(BigDecimal.ZERO) <= 0) continue;
                map.put("UNIT:" + draft.id(), draft.conversionToBase());
            }
        }

        Product source = existing != null ? existing : incoming;
        List<BigDecimal> legacyConversions = legacyConversionValues(source);
        for (int i = 0; i < legacyConversions.size(); i++) {
            map.put("LEGACY:U" + (i + 1), legacyConversions.get(i));
        }
        return map;
    }

    private UnitSelection resolveUnitSelection(String token, Map<String, BigDecimal> tokenConversionMap) {
        String normalized = token == null ? "BASE" : token.trim().toUpperCase(Locale.ROOT);
        if (normalized.isEmpty()) normalized = "BASE";
        BigDecimal conversion = tokenConversionMap.getOrDefault(normalized, BigDecimal.ONE);
        if (conversion.compareTo(BigDecimal.ZERO) <= 0) {
            conversion = BigDecimal.ONE;
        }
        Long unitId = null;
        if (normalized.startsWith("UNIT:")) {
            String raw = normalized.substring("UNIT:".length());
            unitId = parseLong(raw);
        }
        return new UnitSelection(unitId, conversion);
    }

    private void validatePricingRules(Product product) {
        if (product == null) {
            throw new IllegalArgumentException("Product payload is required.");
        }
        if (product.getPrice() != null && product.getPrice().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Retail price cannot be negative.");
        }
        if (product.getWholesalePrice() != null && product.getWholesalePrice().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Wholesale price cannot be negative.");
        }
        if (product.getCostPrice() != null && product.getCostPrice().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Cost price cannot be negative.");
        }
        if (product.getLowStockThreshold() != null && product.getLowStockThreshold() < 0) {
            throw new IllegalArgumentException("Low stock threshold cannot be negative.");
        }
        if (product.getWholesalePrice() == null && product.getWholesaleMinQty() != null) {
            throw new IllegalArgumentException("Wholesale minimum quantity requires wholesale price.");
        }
        if (product.getWholesalePrice() != null) {
            if (product.getWholesaleMinQty() == null || product.getWholesaleMinQty() < 1) {
                throw new IllegalArgumentException("Wholesale minimum quantity must be at least 1.");
            }
        }
    }

    private BigDecimal scaleMoney(BigDecimal amount) {
        if (amount == null) return null;
        if (amount.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Money value cannot be negative.");
        }
        return amount.setScale(2, RoundingMode.HALF_UP);
    }

    private BigDecimal toBaseMoney(BigDecimal amount, BigDecimal conversionToBase) {
        if (amount == null) return null;
        if (amount.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Money value cannot be negative.");
        }
        BigDecimal conversion = conversionToBase == null || conversionToBase.compareTo(BigDecimal.ZERO) <= 0
                ? BigDecimal.ONE
                : conversionToBase;
        return amount.divide(conversion, 2, RoundingMode.HALF_UP);
    }

    private Integer toBaseQty(Integer qty, BigDecimal conversionToBase) {
        if (qty == null) return null;
        if (qty < 0) {
            throw new IllegalArgumentException("Quantity cannot be negative.");
        }
        BigDecimal conversion = conversionToBase == null || conversionToBase.compareTo(BigDecimal.ZERO) <= 0
                ? BigDecimal.ONE
                : conversionToBase;
        BigDecimal normalized = BigDecimal.valueOf(qty)
                .multiply(conversion)
                .setScale(0, RoundingMode.HALF_UP);
        if (normalized.compareTo(BigDecimal.valueOf(Integer.MAX_VALUE)) > 0) {
            throw new IllegalArgumentException("Quantity exceeds maximum supported range.");
        }
        return normalized.intValue();
    }

    private List<BigDecimal> legacyConversionValues(Product source) {
        if (source == null) return List.of();
        List<BigDecimal> values = new ArrayList<>();
        if (source.getUnitsPerBox() != null && source.getUnitsPerBox() > 0) {
            values.add(BigDecimal.valueOf(source.getUnitsPerBox()));
        }
        if (source.getUnitsPerCase() != null && source.getUnitsPerCase() > 0) {
            values.add(BigDecimal.valueOf(source.getUnitsPerCase()));
        }
        return values;
    }

    private Long validateUnitOwnership(Long productId, Long unitId) {
        if (unitId == null || productId == null) return null;
        return productUnitAdminService.belongsToProduct(productId, unitId) ? unitId : null;
    }

    private boolean parseBooleanValue(String value) {
        if (value == null) return false;
        String normalized = value.trim().toLowerCase(Locale.ROOT);
        return "true".equals(normalized) || "1".equals(normalized) || "yes".equals(normalized) || "on".equals(normalized);
    }

    @SafeVarargs
    private final int maxListSize(List<?>... lists) {
        int max = 0;
        if (lists == null) return max;
        for (List<?> list : lists) {
            if (list != null && list.size() > max) {
                max = list.size();
            }
        }
        return max;
    }

    private <T> T at(List<T> list, int index) {
        if (list == null || index < 0 || index >= list.size()) return null;
        return list.get(index);
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
                csv(nullToEmpty(product.getBaseUnitName())),
                csv(product.getBaseUnitPrecision() == null ? "" : String.valueOf(product.getBaseUnitPrecision())),
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
            if (!hasText(product.getBaseUnitName())) {
                product.setBaseUnitName(basicUnitRaw);
            }
        }

        String baseUnitNameRaw = trimToNull(row.get("baseUnitName"));
        if (baseUnitNameRaw != null) {
            product.setBaseUnitName(baseUnitNameRaw);
            product.setBasicUnit(baseUnitNameRaw);
        }
        String baseUnitPrecisionRaw = row.get("baseUnitPrecision");
        if (hasText(baseUnitPrecisionRaw)) {
            Integer baseUnitPrecision = parseInteger(baseUnitPrecisionRaw);
            if (baseUnitPrecision == null || baseUnitPrecision < 0) {
                errors.add("Row " + rowNum + ": invalid base unit precision.");
                return ImportOutcome.FAILED;
            }
            product.setBaseUnitPrecision(baseUnitPrecision);
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
            case "baseunitname" -> "baseUnitName";
            case "baseunitprecision" -> "baseUnitPrecision";
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

    private void addUomFormModel(Model model, Product product) {
        List<ProductUnit> persistedUnits = product == null || product.getId() == null
                ? List.of()
                : productUnitAdminService.listProductUnits(product.getId());
        List<UnitFormRow> rows = new ArrayList<>();
        if (!persistedUnits.isEmpty()) {
            for (ProductUnit unit : persistedUnits) {
                rows.add(new UnitFormRow(
                        unit.getId(),
                        unit.getName(),
                        unit.getAbbreviation(),
                        unit.getConversionToBase(),
                        Boolean.TRUE.equals(unit.getAllowForSale()),
                        Boolean.TRUE.equals(unit.getAllowForPurchase()),
                        Boolean.TRUE.equals(unit.getIsDefaultSaleUnit()),
                        Boolean.TRUE.equals(unit.getIsDefaultPurchaseUnit()),
                        unit.getBarcode(),
                        false
                ));
            }
        } else if (product != null) {
            List<BigDecimal> legacyConversions = legacyConversionValues(product);
            for (int i = 0; i < legacyConversions.size(); i++) {
                rows.add(new UnitFormRow(
                        null,
                        "Legacy Unit " + (i + 1),
                        null,
                        legacyConversions.get(i),
                        true,
                        true,
                        i == 0,
                        i == 0,
                        null,
                        true
                ));
            }
        }

        int defaultSaleIndex = defaultIndex(rows, true);
        int defaultPurchaseIndex = defaultIndex(rows, false);
        model.addAttribute("uomRows", rows);
        model.addAttribute("uomDefaultSaleIndex", defaultSaleIndex);
        model.addAttribute("uomDefaultPurchaseIndex", defaultPurchaseIndex);

        String baseUnitName = (product == null || !hasText(product.getBaseUnitName()))
                ? "piece"
                : product.getBaseUnitName();
        List<String> baseUnitSuggestions = new ArrayList<>(masterStockUnitService.listActiveUnitNames());
        if (hasText(baseUnitName)
                && baseUnitSuggestions.stream().noneMatch(option -> option.equalsIgnoreCase(baseUnitName))) {
            baseUnitSuggestions.add(0, baseUnitName);
        }
        model.addAttribute("baseUnitSuggestions", baseUnitSuggestions);
        Map<String, BigDecimal> conversionByToken = new LinkedHashMap<>();
        List<UnitOptionView> options = new ArrayList<>();
        options.add(new UnitOptionView("BASE", baseUnitName + " (base)", BigDecimal.ONE));
        conversionByToken.put("BASE", BigDecimal.ONE);

        if (!persistedUnits.isEmpty()) {
            for (ProductUnit unit : persistedUnits) {
                if (unit.getId() == null || unit.getConversionToBase() == null || unit.getConversionToBase().compareTo(BigDecimal.ZERO) <= 0) {
                    continue;
                }
                String token = "UNIT:" + unit.getId();
                String label = unit.getName();
                if (hasText(unit.getAbbreviation())) {
                    label = label + " (" + unit.getAbbreviation() + ")";
                }
                options.add(new UnitOptionView(token, label, unit.getConversionToBase()));
                conversionByToken.put(token, unit.getConversionToBase());
            }
        } else {
            int legacyIndex = 1;
            for (UnitFormRow row : rows) {
                if (!hasText(row.name()) || row.conversionToBase() == null || row.conversionToBase().compareTo(BigDecimal.ZERO) <= 0) {
                    continue;
                }
                String token = "LEGACY:U" + legacyIndex++;
                options.add(new UnitOptionView(token, row.name() + " (legacy)", row.conversionToBase()));
                conversionByToken.put(token, row.conversionToBase());
            }
        }

        model.addAttribute("uomUnitOptions", options);
        String retailToken = tokenForUnitId(product == null ? null : product.getRetailPriceUnitId(), conversionByToken);
        String wholesaleToken = tokenForUnitId(product == null ? null : product.getWholesalePriceUnitId(), conversionByToken);
        String wholesaleMinToken = tokenForUnitId(product == null ? null : product.getWholesaleMinQtyUnitId(), conversionByToken);
        String lowStockToken = tokenForUnitId(product == null ? null : product.getLowStockThresholdUnitId(), conversionByToken);
        model.addAttribute("retailPriceUnitToken", retailToken);
        model.addAttribute("wholesalePriceUnitToken", wholesaleToken);
        model.addAttribute("wholesaleMinQtyUnitToken", wholesaleMinToken);
        model.addAttribute("lowStockThresholdUnitToken", lowStockToken);

        BigDecimal retailAmount = fromBaseMoney(product == null ? null : product.getPrice(), conversionByToken.get(retailToken));
        BigDecimal wholesaleAmount = fromBaseMoney(product == null ? null : product.getWholesalePrice(), conversionByToken.get(wholesaleToken));
        Integer wholesaleMinAmount = fromBaseQty(product == null ? null : product.getWholesaleMinQty(), conversionByToken.get(wholesaleMinToken));
        Integer lowStockAmount = fromBaseQty(product == null ? null : product.getLowStockThreshold(), conversionByToken.get(lowStockToken));
        model.addAttribute("retailPriceDisplay", retailAmount);
        model.addAttribute("wholesalePriceDisplay", wholesaleAmount);
        model.addAttribute("wholesaleMinQtyDisplay", wholesaleMinAmount);
        model.addAttribute("lowStockThresholdDisplay", lowStockAmount);
    }

    private int defaultIndex(List<UnitFormRow> rows, boolean sale) {
        if (rows == null || rows.isEmpty()) return -1;
        for (int i = 0; i < rows.size(); i++) {
            UnitFormRow row = rows.get(i);
            if (sale && row.defaultSale()) return i;
            if (!sale && row.defaultPurchase()) return i;
        }
        return 0;
    }

    private String tokenForUnitId(Long unitId, Map<String, BigDecimal> conversionByToken) {
        if (unitId == null) return "BASE";
        String token = "UNIT:" + unitId;
        if (conversionByToken.containsKey(token)) return token;
        return "BASE";
    }

    private BigDecimal fromBaseMoney(BigDecimal baseAmount, BigDecimal conversionToBase) {
        if (baseAmount == null) return null;
        BigDecimal conversion = conversionToBase == null || conversionToBase.compareTo(BigDecimal.ZERO) <= 0
                ? BigDecimal.ONE
                : conversionToBase;
        return baseAmount.multiply(conversion).setScale(2, RoundingMode.HALF_UP).stripTrailingZeros();
    }

    private Integer fromBaseQty(Integer baseQty, BigDecimal conversionToBase) {
        if (baseQty == null) return null;
        BigDecimal conversion = conversionToBase == null || conversionToBase.compareTo(BigDecimal.ZERO) <= 0
                ? BigDecimal.ONE
                : conversionToBase;
        return BigDecimal.valueOf(Math.max(0, baseQty))
                .divide(conversion, 6, RoundingMode.HALF_UP)
                .setScale(0, RoundingMode.HALF_UP)
                .intValue();
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
        if ((unitsPerBox == null || unitsPerBox <= 0 || unitsPerCase == null || unitsPerCase <= 0)
                && product.getId() != null) {
            List<ProductUnit> productUnits = productUnitAdminService.listProductUnits(product.getId());
            LegacyPackSizes inferred = inferLegacyPackSizes(productUnits);
            if ((unitsPerBox == null || unitsPerBox <= 0) && inferred.first() != null) {
                unitsPerBox = inferred.first();
            }
            if ((unitsPerCase == null || unitsPerCase <= 0) && inferred.second() != null) {
                unitsPerCase = inferred.second();
            }
        }

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

    private Integer integerConversion(BigDecimal value) {
        if (value == null || value.compareTo(BigDecimal.ZERO) <= 0) return null;
        try {
            return value.intValueExact();
        } catch (ArithmeticException ex) {
            return null;
        }
    }

    private LegacyPackSizes inferLegacyPackSizes(List<ProductUnit> units) {
        if (units == null || units.isEmpty()) {
            return new LegacyPackSizes(null, null);
        }
        Integer min = null;
        Integer max = null;
        for (ProductUnit unit : units) {
            if (unit == null || unit.getConversionToBase() == null || unit.getConversionToBase().compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            Integer conversion = integerConversion(unit.getConversionToBase());
            if (conversion == null) continue;
            if (min == null || conversion < min) {
                min = conversion;
            }
            if (max == null || conversion > max) {
                max = conversion;
            }
        }
        if (min != null && min.equals(max)) {
            max = null;
        }
        return new LegacyPackSizes(min, max);
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

    private record LegacyPackSizes(Integer first, Integer second) {}

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

    private record UnitFormRow(
            Long id,
            String name,
            String abbreviation,
            BigDecimal conversionToBase,
            boolean allowForSale,
            boolean allowForPurchase,
            boolean defaultSale,
            boolean defaultPurchase,
            String barcode,
            boolean legacy
    ) {
    }

    private record UnitOptionView(
            String token,
            String label,
            BigDecimal conversionToBase
    ) {
    }

    private record UnitSelection(
            Long unitId,
            BigDecimal conversionToBase
    ) {
    }

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
