package com.devcore.pos_system.api.v1.controller;

import com.devcore.pos_system.api.v1.dto.common.ApiEnvelope;
import com.devcore.pos_system.api.v1.dto.supplier.ApiSupplierDto;
import com.devcore.pos_system.api.v1.dto.supplier.SupplierUpsertRequest;
import com.devcore.pos_system.api.v1.service.ApiSupplierService;
import com.devcore.pos_system.api.v1.support.ApiTrace;
import com.devcore.pos_system.entity.SupplierStatus;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/api/v1/suppliers")
public class SupplierApiV1Controller {
    private final ApiSupplierService apiSupplierService;

    public SupplierApiV1Controller(ApiSupplierService apiSupplierService) {
        this.apiSupplierService = apiSupplierService;
    }

    @GetMapping
    public ApiEnvelope<List<ApiSupplierDto>> list(@RequestParam(required = false) String q,
                                                  @RequestParam(required = false) SupplierStatus status,
                                                  HttpServletRequest request) {
        return ApiEnvelope.success(
                "SUPPLIERS_LIST_OK",
                "Suppliers fetched successfully.",
                apiSupplierService.list(q, status),
                ApiTrace.resolve(request)
        );
    }

    @GetMapping("/{id}")
    public ApiEnvelope<ApiSupplierDto> getById(@PathVariable Long id, HttpServletRequest request) {
        return ApiEnvelope.success(
                "SUPPLIER_FETCH_OK",
                "Supplier fetched successfully.",
                apiSupplierService.getById(id),
                ApiTrace.resolve(request)
        );
    }

    @PostMapping
    public ResponseEntity<ApiEnvelope<ApiSupplierDto>> create(@Valid @RequestBody SupplierUpsertRequest requestBody,
                                                              HttpServletRequest request) {
        return ResponseEntity.status(HttpStatus.CREATED).body(
                ApiEnvelope.success(
                        "SUPPLIER_CREATE_OK",
                        "Supplier created successfully.",
                        apiSupplierService.create(requestBody),
                        ApiTrace.resolve(request)
                )
        );
    }

    @PutMapping("/{id}")
    public ApiEnvelope<ApiSupplierDto> update(@PathVariable Long id,
                                              @Valid @RequestBody SupplierUpsertRequest requestBody,
                                              HttpServletRequest request) {
        return ApiEnvelope.success(
                "SUPPLIER_UPDATE_OK",
                "Supplier updated successfully.",
                apiSupplierService.update(id, requestBody),
                ApiTrace.resolve(request)
        );
    }

    @DeleteMapping("/{id}")
    public ApiEnvelope<Void> delete(@PathVariable Long id, HttpServletRequest request) {
        apiSupplierService.delete(id);
        return ApiEnvelope.success(
                "SUPPLIER_DELETE_OK",
                "Supplier deleted successfully.",
                null,
                ApiTrace.resolve(request)
        );
    }
}
