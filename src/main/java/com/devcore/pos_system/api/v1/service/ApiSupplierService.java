package com.devcore.pos_system.api.v1.service;

import com.devcore.pos_system.api.v1.dto.supplier.ApiSupplierDto;
import com.devcore.pos_system.api.v1.dto.supplier.SupplierUpsertRequest;
import com.devcore.pos_system.entity.SupplierStatus;

import java.util.List;

public interface ApiSupplierService {
    List<ApiSupplierDto> list(String q, SupplierStatus status);

    ApiSupplierDto getById(Long id);

    ApiSupplierDto create(SupplierUpsertRequest request);

    ApiSupplierDto update(Long id, SupplierUpsertRequest request);

    void delete(Long id);
}
