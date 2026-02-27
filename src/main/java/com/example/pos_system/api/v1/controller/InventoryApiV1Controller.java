package com.example.pos_system.api.v1.controller;

import com.example.pos_system.api.v1.dto.common.ApiEnvelope;
import com.example.pos_system.api.v1.dto.common.ApiPageData;
import com.example.pos_system.api.v1.dto.inventory.StockAdjustmentRequest;
import com.example.pos_system.api.v1.dto.inventory.StockAvailabilityDto;
import com.example.pos_system.api.v1.dto.inventory.StockMovementDto;
import com.example.pos_system.api.v1.dto.inventory.StockReceiveRequest;
import com.example.pos_system.api.v1.service.ApiInventoryService;
import com.example.pos_system.api.v1.support.ApiTrace;
import com.example.pos_system.entity.StockMovementType;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.format.annotation.DateTimeFormat;

import java.time.LocalDate;

@RestController
@RequestMapping("/api/v1/inventory")
public class InventoryApiV1Controller {
    private static final int DEFAULT_PAGE_SIZE = 20;
    private static final int MAX_PAGE_SIZE = 200;

    private final ApiInventoryService apiInventoryService;

    public InventoryApiV1Controller(ApiInventoryService apiInventoryService) {
        this.apiInventoryService = apiInventoryService;
    }

    @GetMapping("/movements")
    public ApiEnvelope<ApiPageData<StockMovementDto>> listMovements(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
            @RequestParam(required = false) Long productId,
            @RequestParam(required = false) StockMovementType type,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(defaultValue = "createdAt") String sort,
            @RequestParam(defaultValue = "desc") String dir,
            HttpServletRequest request) {
        int safePage = Math.max(0, page);
        int safeSize = normalizePageSize(size);
        Page<StockMovementDto> movementPage = apiInventoryService.listMovements(
                from,
                to,
                productId,
                type,
                PageRequest.of(safePage, safeSize, sortBy(sort, dir))
        );
        return ApiEnvelope.success(
                "INVENTORY_MOVEMENT_LIST_OK",
                "Inventory movements fetched successfully.",
                ApiPageData.from(movementPage),
                ApiTrace.resolve(request)
        );
    }

    @GetMapping("/products/{productId}/availability")
    public ApiEnvelope<StockAvailabilityDto> getAvailability(@PathVariable Long productId,
                                                             HttpServletRequest request) {
        return ApiEnvelope.success(
                "INVENTORY_AVAILABILITY_OK",
                "Stock availability fetched successfully.",
                apiInventoryService.getAvailability(productId),
                ApiTrace.resolve(request)
        );
    }

    @PostMapping("/adjustments")
    public ApiEnvelope<StockAvailabilityDto> adjust(@Valid @RequestBody StockAdjustmentRequest requestBody,
                                                    HttpServletRequest request) {
        return ApiEnvelope.success(
                "INVENTORY_ADJUSTMENT_OK",
                "Stock adjusted successfully.",
                apiInventoryService.adjustStock(requestBody),
                ApiTrace.resolve(request)
        );
    }

    @PostMapping("/receive")
    public ApiEnvelope<StockAvailabilityDto> receive(@Valid @RequestBody StockReceiveRequest requestBody,
                                                     HttpServletRequest request) {
        return ApiEnvelope.success(
                "INVENTORY_RECEIVE_OK",
                "Stock received successfully.",
                apiInventoryService.receiveStock(requestBody),
                ApiTrace.resolve(request)
        );
    }

    private Sort sortBy(String sort, String dir) {
        String property = (sort == null || sort.isBlank()) ? "createdAt" : sort.trim();
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
