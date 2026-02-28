package com.devcore.pos_system.api.v1.service.impl;

import com.devcore.pos_system.api.v1.dto.supplier.ApiSupplierDto;
import com.devcore.pos_system.api.v1.dto.supplier.SupplierUpsertRequest;
import com.devcore.pos_system.api.v1.exception.ApiBadRequestException;
import com.devcore.pos_system.api.v1.exception.ApiNotFoundException;
import com.devcore.pos_system.api.v1.mapper.ApiV1Mapper;
import com.devcore.pos_system.api.v1.service.ApiSupplierService;
import com.devcore.pos_system.entity.Supplier;
import com.devcore.pos_system.entity.SupplierStatus;
import com.devcore.pos_system.service.SupplierService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Locale;

@Service
@Transactional
public class ApiSupplierServiceImpl implements ApiSupplierService {
    private final SupplierService supplierService;
    private final ApiV1Mapper mapper;

    public ApiSupplierServiceImpl(SupplierService supplierService, ApiV1Mapper mapper) {
        this.supplierService = supplierService;
        this.mapper = mapper;
    }

    @Override
    @Transactional(readOnly = true)
    public List<ApiSupplierDto> list(String q, SupplierStatus status) {
        return supplierService.list(q, status).stream().map(mapper::toSupplierDto).toList();
    }

    @Override
    @Transactional(readOnly = true)
    public ApiSupplierDto getById(Long id) {
        if (id == null) {
            throw new ApiBadRequestException("supplier id is required.");
        }
        Supplier supplier = supplierService.get(id);
        if (supplier == null) {
            throw new ApiNotFoundException("supplier not found.");
        }
        return mapper.toSupplierDto(supplier);
    }

    @Override
    public ApiSupplierDto create(SupplierUpsertRequest request) {
        try {
            Supplier saved = supplierService.save(
                    null,
                    request.name(),
                    request.phone(),
                    request.email(),
                    request.address(),
                    request.status()
            );
            return mapper.toSupplierDto(saved);
        } catch (IllegalArgumentException ex) {
            throw toApiException(ex);
        }
    }

    @Override
    public ApiSupplierDto update(Long id, SupplierUpsertRequest request) {
        if (id == null) {
            throw new ApiBadRequestException("supplier id is required.");
        }
        try {
            Supplier saved = supplierService.save(
                    id,
                    request.name(),
                    request.phone(),
                    request.email(),
                    request.address(),
                    request.status()
            );
            return mapper.toSupplierDto(saved);
        } catch (IllegalArgumentException ex) {
            throw toApiException(ex);
        }
    }

    @Override
    public void delete(Long id) {
        if (id == null) {
            throw new ApiBadRequestException("supplier id is required.");
        }
        try {
            supplierService.delete(id);
        } catch (IllegalArgumentException ex) {
            throw toApiException(ex);
        }
    }

    private RuntimeException toApiException(IllegalArgumentException ex) {
        String message = ex.getMessage() == null ? "Invalid supplier request." : ex.getMessage();
        if (message.toLowerCase(Locale.ROOT).contains("not found")) {
            return new ApiNotFoundException(message);
        }
        return new ApiBadRequestException(message);
    }
}
