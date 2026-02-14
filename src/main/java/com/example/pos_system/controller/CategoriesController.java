package com.example.pos_system.controller;

import com.example.pos_system.entity.Category;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.ProductRepo;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Controller
@RequestMapping("/categories")
public class CategoriesController {
    private static final int PAGE_SIZE = 20;
    private final CategoryRepo categoryRepo;
    private final ProductRepo productRepo;

    public CategoriesController(CategoryRepo categoryRepo, ProductRepo productRepo) {
        this.categoryRepo = categoryRepo;
        this.productRepo = productRepo;
    }

    @GetMapping
    public String list(@RequestParam(required = false) String q,
                       @RequestParam(required = false) String error,
                       @RequestParam(defaultValue = "0") int page,
                       @RequestParam(required = false) String status,
                       @RequestParam(required = false, defaultValue = "20") Integer size,
                       Model model) {
        int pageNum = Math.max(0, page);
        int pageSize = Math.max(5, Math.min(100, size == null ? PAGE_SIZE : size));
        Boolean active = null;
        if ("active".equalsIgnoreCase(status)) {
            active = true;
        } else if ("inactive".equalsIgnoreCase(status)) {
            active = false;
        }
        Pageable pageable = PageRequest.of(pageNum, pageSize,
                Sort.by("sortOrder").ascending().and(Sort.by("name").ascending()));
        Page<Category> categoryPage = categoryRepo.search(q, active, pageable);
        model.addAttribute("categories", categoryPage.getContent());
        model.addAttribute("page", pageNum);
        model.addAttribute("totalPages", Math.max(1, categoryPage.getTotalPages()));
        model.addAttribute("hasNext", categoryPage.hasNext());
        model.addAttribute("hasPrev", categoryPage.hasPrevious());
        model.addAttribute("nextPage", pageNum + 1);
        model.addAttribute("prevPage", Math.max(0, pageNum - 1));
        model.addAttribute("q", q);
        model.addAttribute("pageSize", pageSize);
        model.addAttribute("status", status);

        Map<Long, Long> productCounts = new HashMap<>();
        for (ProductRepo.CategoryCount row : productRepo.countByCategory()) {
            if (row.getCategoryId() != null) {
                productCounts.put(row.getCategoryId(), row.getCount());
            }
        }
        Map<Long, Long> lowStockCounts = new HashMap<>();
        for (ProductRepo.CategoryCount row : productRepo.countLowStockByCategory()) {
            if (row.getCategoryId() != null) {
                lowStockCounts.put(row.getCategoryId(), row.getCount());
            }
        }
        model.addAttribute("productCounts", productCounts);
        model.addAttribute("lowStockCounts", lowStockCounts);
        model.addAttribute("totalCategories", categoryRepo.count());
        model.addAttribute("activeCategories", categoryRepo.countByActiveTrue());
        model.addAttribute("inactiveCategories", categoryRepo.countByActiveFalse());
        model.addAttribute("totalProducts", productRepo.count());
        model.addAttribute("lowStockProducts", productRepo.countLowStock());
        model.addAttribute("categoriesWithProducts", productCounts.size());

        if ("inuse".equals(error)) {
            model.addAttribute("error", "Cannot delete a category that has products.");
        }
        if ("invalidImage".equals(error)) {
            model.addAttribute("error", "Please upload a valid image file.");
        }
        if ("uploadFailed".equals(error)) {
            model.addAttribute("error", "Image upload failed. Please try again.");
        }
        if ("uploadTooLarge".equals(error)) {
            model.addAttribute("error", "Image is too large. Maximum upload size is 10MB.");
        }
        return "categories/list";
    }

    @GetMapping("/new")
    public String createForm(Model model) {
        model.addAttribute("category", new Category());
        return "categories/form";
    }

    @GetMapping("/{id}/edit")
    public String editForm(@PathVariable Long id, Model model) {
        Category category = categoryRepo.findById(id).orElseThrow();
        model.addAttribute("category", category);
        return "categories/form";
    }

    @PostMapping
    public String save(@ModelAttribute Category category,
                       @RequestParam(required = false) MultipartFile imageFile) {
        normalizeEmptyStrings(category);
        if (category.getActive() == null) {
            category.setActive(false);
        }
        if (category.getSortOrder() == null) {
            Integer max = categoryRepo.findMaxSortOrder();
            category.setSortOrder(max == null ? 0 : max + 1);
        }
        if (imageFile != null && !imageFile.isEmpty()) {
            if (imageFile.getContentType() == null || !imageFile.getContentType().startsWith("image/")) {
                return "redirect:/categories?error=invalidImage";
            }
            String imageUrl = storeImage(imageFile);
            if (imageUrl == null) {
                return "redirect:/categories?error=uploadFailed";
            }
            category.setImageUrl(imageUrl);
        }
        categoryRepo.save(category);
        return "redirect:/categories";
    }

    @PostMapping("/{id}/delete")
    public String delete(@PathVariable Long id,
                         @RequestParam(required = false) String q,
                         @RequestParam(defaultValue = "0") int page,
                         @RequestParam(required = false) String status,
                         @RequestParam(required = false, defaultValue = "20") Integer size) {
        StringBuilder redirect = new StringBuilder("redirect:/categories?page=" + Math.max(0, page));
        if (q != null && !q.isBlank()) {
            redirect.append("&q=").append(org.springframework.web.util.UriUtils.encode(q, java.nio.charset.StandardCharsets.UTF_8));
        }
        if (status != null && !status.isBlank()) {
            redirect.append("&status=").append(org.springframework.web.util.UriUtils.encode(status, java.nio.charset.StandardCharsets.UTF_8));
        }
        if (size != null) {
            redirect.append("&size=").append(size);
        }
        if (productRepo.existsByCategory_Id(id)) {
            redirect.append("&error=inuse");
            return redirect.toString();
        }
        categoryRepo.deleteById(id);
        return redirect.toString();
    }

    @PostMapping("/{id}/toggle-active")
    public String toggleActive(@PathVariable Long id,
                               @RequestParam(required = false) String q,
                               @RequestParam(defaultValue = "0") int page,
                               @RequestParam(required = false) String status,
                               @RequestParam(required = false, defaultValue = "20") Integer size) {
        Category category = categoryRepo.findById(id).orElseThrow();
        category.setActive(!Boolean.TRUE.equals(category.getActive()));
        categoryRepo.save(category);
        StringBuilder redirect = new StringBuilder("redirect:/categories?page=" + Math.max(0, page));
        if (q != null && !q.isBlank()) {
            redirect.append("&q=").append(org.springframework.web.util.UriUtils.encode(q, java.nio.charset.StandardCharsets.UTF_8));
        }
        if (status != null && !status.isBlank()) {
            redirect.append("&status=").append(org.springframework.web.util.UriUtils.encode(status, java.nio.charset.StandardCharsets.UTF_8));
        }
        if (size != null) {
            redirect.append("&size=").append(size);
        }
        return redirect.toString();
    }

    @PostMapping("/bulk")
    public String bulkAction(@RequestParam String action,
                             @RequestParam(required = false) List<Long> ids,
                             @RequestParam(required = false) String q,
                             @RequestParam(required = false) String status,
                             @RequestParam(required = false, defaultValue = "20") Integer size,
                             RedirectAttributes redirectAttributes) {
        if (ids == null || ids.isEmpty()) {
            redirectAttributes.addFlashAttribute("error", "Select at least one category.");
            StringBuilder redirect = new StringBuilder("redirect:/categories");
            boolean hasParam = false;
            if (q != null && !q.isBlank()) {
                redirect.append("?q=").append(org.springframework.web.util.UriUtils.encode(q, java.nio.charset.StandardCharsets.UTF_8));
                hasParam = true;
            }
            if (status != null && !status.isBlank()) {
                redirect.append(hasParam ? "&" : "?")
                        .append("status=").append(org.springframework.web.util.UriUtils.encode(status, java.nio.charset.StandardCharsets.UTF_8));
                hasParam = true;
            }
            if (size != null) {
                redirect.append(hasParam ? "&" : "?").append("size=").append(size);
            }
            return redirect.toString();
        }
        if ("delete".equals(action)) {
            int blocked = 0;
            for (Long id : ids) {
                if (productRepo.existsByCategory_Id(id)) {
                    blocked++;
                    continue;
                }
                categoryRepo.deleteById(id);
            }
            if (blocked > 0) {
                redirectAttributes.addFlashAttribute("error", "Some categories could not be deleted because they have products.");
            }
        } else if ("deactivate".equals(action)) {
            for (Long id : ids) {
                categoryRepo.findById(id).ifPresent(c -> {
                    c.setActive(false);
                    categoryRepo.save(c);
                });
            }
        } else if ("activate".equals(action)) {
            for (Long id : ids) {
                categoryRepo.findById(id).ifPresent(c -> {
                    c.setActive(true);
                    categoryRepo.save(c);
                });
            }
        }
        StringBuilder redirect = new StringBuilder("redirect:/categories");
        boolean hasParam = false;
        if (q != null && !q.isBlank()) {
            redirect.append("?q=").append(org.springframework.web.util.UriUtils.encode(q, java.nio.charset.StandardCharsets.UTF_8));
            hasParam = true;
        }
        if (status != null && !status.isBlank()) {
            redirect.append(hasParam ? "&" : "?")
                    .append("status=").append(org.springframework.web.util.UriUtils.encode(status, java.nio.charset.StandardCharsets.UTF_8));
            hasParam = true;
        }
        if (size != null) {
            redirect.append(hasParam ? "&" : "?").append("size=").append(size);
        }
        return redirect.toString();
    }

    @PostMapping("/reorder")
    @ResponseBody
    public ResponseEntity<Void> reorder(@RequestBody ReorderRequest request) {
        if (request == null || request.ids == null || request.ids.isEmpty()) {
            return ResponseEntity.badRequest().build();
        }
        int page = request.page == null ? 0 : Math.max(0, request.page);
        int pageSize = request.pageSize == null ? PAGE_SIZE : Math.max(1, request.pageSize);
        Map<Long, Category> byId = new HashMap<>();
        for (Category c : categoryRepo.findAllById(request.ids)) {
            byId.put(c.getId(), c);
        }
        int order = page * pageSize + 1;
        for (Long id : request.ids) {
            Category c = byId.get(id);
            if (c != null) {
                c.setSortOrder(order++);
            }
        }
        categoryRepo.saveAll(byId.values());
        return ResponseEntity.ok().build();
    }

    public static class ReorderRequest {
        public List<Long> ids;
        public Integer page;
        public Integer pageSize;
    }

    private String storeImage(MultipartFile imageFile) {
        String originalName = imageFile.getOriginalFilename();
        String ext = "";
        if (originalName != null) {
            int dot = originalName.lastIndexOf('.');
            if (dot >= 0) ext = originalName.substring(dot);
        }
        String filename = UUID.randomUUID().toString().replace("-", "") + ext;
        Path uploadDir = Paths.get("src/main/resources/static/uploads/categories");
        Path targetDir = Paths.get("target/classes/static/uploads/categories");
        try {
            Files.createDirectories(uploadDir);
            Path saved = uploadDir.resolve(filename);
            Files.copy(imageFile.getInputStream(), saved, StandardCopyOption.REPLACE_EXISTING);
            try {
                Files.createDirectories(targetDir);
                Files.copy(saved, targetDir.resolve(filename), StandardCopyOption.REPLACE_EXISTING);
            } catch (IOException ignored) {
                // best effort
            }
        } catch (IOException e) {
            return null;
        }
        return "/uploads/categories/" + filename;
    }

    private void normalizeEmptyStrings(Category category) {
        if (category.getName() != null && category.getName().trim().isEmpty()) {
            category.setName(null);
        }
        if (category.getDescription() != null && category.getDescription().trim().isEmpty()) {
            category.setDescription(null);
        }
        if (category.getImageUrl() != null && category.getImageUrl().trim().isEmpty()) {
            category.setImageUrl(null);
        }
    }
}
