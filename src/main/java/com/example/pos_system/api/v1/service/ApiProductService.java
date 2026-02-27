package com.example.pos_system.api.v1.service;

import com.example.pos_system.api.v1.dto.product.ApiProductDto;
import com.example.pos_system.api.v1.dto.product.ProductCreateRequest;
import com.example.pos_system.api.v1.dto.product.ProductUpdateRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface ApiProductService {
    Page<ApiProductDto> list(String q, Long categoryId, Boolean active, Boolean lowStock, Pageable pageable);

    ApiProductDto getById(Long id);

    ApiProductDto create(ProductCreateRequest request);

    ApiProductDto update(Long id, ProductUpdateRequest request);
}
