package com.example.pos_system.api.v1.controller;

import com.example.pos_system.api.v1.dto.common.ApiEnvelope;
import com.example.pos_system.api.v1.dto.common.ApiPageData;
import com.example.pos_system.api.v1.dto.product.ApiProductDto;
import com.example.pos_system.api.v1.dto.product.ProductCreateRequest;
import com.example.pos_system.api.v1.dto.product.ProductUpdateRequest;
import com.example.pos_system.api.v1.service.ApiProductService;
import com.example.pos_system.api.v1.support.ApiTrace;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/products")
public class ProductApiV1Controller {
    private static final int DEFAULT_PAGE_SIZE = 20;
    private static final int MAX_PAGE_SIZE = 200;

    private final ApiProductService apiProductService;

    public ProductApiV1Controller(ApiProductService apiProductService) {
        this.apiProductService = apiProductService;
    }

    @GetMapping
    public ApiEnvelope<ApiPageData<ApiProductDto>> list(@RequestParam(required = false) String q,
                                                        @RequestParam(required = false) Long categoryId,
                                                        @RequestParam(required = false) Boolean active,
                                                        @RequestParam(required = false) Boolean lowStock,
                                                        @RequestParam(defaultValue = "0") int page,
                                                        @RequestParam(defaultValue = "20") int size,
                                                        @RequestParam(defaultValue = "id") String sort,
                                                        @RequestParam(defaultValue = "desc") String dir,
                                                        HttpServletRequest request) {
        int safePage = Math.max(0, page);
        int safeSize = normalizePageSize(size);
        Page<ApiProductDto> products = apiProductService.list(
                q,
                categoryId,
                active,
                lowStock,
                PageRequest.of(safePage, safeSize, sortBy(sort, dir))
        );
        return ApiEnvelope.success(
                "PRODUCT_LIST_OK",
                "Products fetched successfully.",
                ApiPageData.from(products),
                ApiTrace.resolve(request)
        );
    }

    @GetMapping("/{id}")
    public ApiEnvelope<ApiProductDto> getById(@PathVariable Long id, HttpServletRequest request) {
        return ApiEnvelope.success(
                "PRODUCT_FETCH_OK",
                "Product fetched successfully.",
                apiProductService.getById(id),
                ApiTrace.resolve(request)
        );
    }

    @PostMapping
    public ResponseEntity<ApiEnvelope<ApiProductDto>> create(@Valid @RequestBody ProductCreateRequest requestBody,
                                                             HttpServletRequest request) {
        ApiProductDto created = apiProductService.create(requestBody);
        return ResponseEntity.status(HttpStatus.CREATED).body(
                ApiEnvelope.success(
                        "PRODUCT_CREATE_OK",
                        "Product created successfully.",
                        created,
                        ApiTrace.resolve(request)
                )
        );
    }

    @PutMapping("/{id}")
    public ApiEnvelope<ApiProductDto> update(@PathVariable Long id,
                                             @Valid @RequestBody ProductUpdateRequest requestBody,
                                             HttpServletRequest request) {
        return ApiEnvelope.success(
                "PRODUCT_UPDATE_OK",
                "Product updated successfully.",
                apiProductService.update(id, requestBody),
                ApiTrace.resolve(request)
        );
    }

    private Sort sortBy(String sort, String dir) {
        String property = (sort == null || sort.isBlank()) ? "id" : sort.trim();
        Sort.Direction direction = "asc".equalsIgnoreCase(dir) ? Sort.Direction.ASC : Sort.Direction.DESC;
        return Sort.by(direction, property);
    }

    private int normalizePageSize(int requested) {
        if (requested <= 0) {
            return DEFAULT_PAGE_SIZE;
        }
        return Math.min(requested, MAX_PAGE_SIZE);
    }
}
