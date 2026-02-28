package com.devcore.pos_system.api.v1.service;

import com.devcore.pos_system.api.v1.dto.product.ApiProductDto;
import com.devcore.pos_system.api.v1.dto.product.ApiProductUnitDto;
import com.devcore.pos_system.api.v1.dto.product.ProductCreateRequest;
import com.devcore.pos_system.api.v1.dto.product.ProductUnitUpsertRequest;
import com.devcore.pos_system.api.v1.dto.product.ProductUpdateRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface ApiProductService {
    Page<ApiProductDto> list(String q, Long categoryId, Boolean active, Boolean lowStock, Pageable pageable);

    ApiProductDto getById(Long id);

    ApiProductDto create(ProductCreateRequest request);

    ApiProductDto update(Long id, ProductUpdateRequest request);

    List<ApiProductUnitDto> listUnits(Long productId);

    ApiProductUnitDto createUnit(Long productId, ProductUnitUpsertRequest request);

    ApiProductUnitDto updateUnit(Long productId, Long unitId, ProductUnitUpsertRequest request);

    void deleteUnit(Long productId, Long unitId);
}
