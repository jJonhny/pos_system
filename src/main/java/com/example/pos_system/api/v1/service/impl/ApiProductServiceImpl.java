package com.example.pos_system.api.v1.service.impl;

import com.example.pos_system.api.v1.dto.product.ApiProductDto;
import com.example.pos_system.api.v1.dto.product.ProductCreateRequest;
import com.example.pos_system.api.v1.dto.product.ProductUpdateRequest;
import com.example.pos_system.api.v1.exception.ApiBadRequestException;
import com.example.pos_system.api.v1.exception.ApiNotFoundException;
import com.example.pos_system.api.v1.mapper.ApiV1Mapper;
import com.example.pos_system.api.v1.service.ApiProductService;
import com.example.pos_system.entity.Category;
import com.example.pos_system.entity.Product;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.ProductRepo;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Locale;

@Service
@Transactional
public class ApiProductServiceImpl implements ApiProductService {
    private final ProductRepo productRepo;
    private final CategoryRepo categoryRepo;
    private final ApiV1Mapper mapper;

    public ApiProductServiceImpl(ProductRepo productRepo,
                                 CategoryRepo categoryRepo,
                                 ApiV1Mapper mapper) {
        this.productRepo = productRepo;
        this.categoryRepo = categoryRepo;
        this.mapper = mapper;
    }

    @Override
    @Transactional(readOnly = true)
    public Page<ApiProductDto> list(String q, Long categoryId, Boolean active, Boolean lowStock, Pageable pageable) {
        Specification<Product> specification = buildSpecification(q, categoryId, active, lowStock);
        return productRepo.findAll(specification, pageable).map(mapper::toProductDto);
    }

    @Override
    @Transactional(readOnly = true)
    public ApiProductDto getById(Long id) {
        return mapper.toProductDto(requireProduct(id));
    }

    @Override
    public ApiProductDto create(ProductCreateRequest request) {
        Product product = new Product();
        product.setSku(normalize(request.sku()));
        product.setBarcode(normalize(request.barcode()));
        product.setName(requireText(request.name(), "name is required."));
        product.setPrice(request.price());
        product.setCostPrice(request.costPrice());
        product.setLowStockThreshold(request.lowStockThreshold() == null ? 0 : request.lowStockThreshold());
        product.setActive(request.active() == null || request.active());
        product.setAllowNegativeStock(request.allowNegativeStock() != null && request.allowNegativeStock());
        product.setImageUrl(normalize(request.imageUrl()));

        // New products start with zero stock; receiving/PO APIs should increase on-hand stock.
        product.setStockQty(0);

        if (request.categoryId() != null) {
            product.setCategory(requireCategory(request.categoryId()));
        }

        Product saved = productRepo.save(product);
        return mapper.toProductDto(saved);
    }

    @Override
    public ApiProductDto update(Long id, ProductUpdateRequest request) {
        Product product = requireProduct(id);

        if (request.sku() != null) {
            product.setSku(normalize(request.sku()));
        }
        if (request.barcode() != null) {
            product.setBarcode(normalize(request.barcode()));
        }
        if (request.name() != null) {
            product.setName(requireText(request.name(), "name cannot be blank."));
        }
        if (request.price() != null) {
            product.setPrice(request.price());
        }
        if (request.costPrice() != null) {
            product.setCostPrice(request.costPrice());
        }
        if (request.lowStockThreshold() != null) {
            product.setLowStockThreshold(request.lowStockThreshold());
        }
        if (request.active() != null) {
            product.setActive(request.active());
        }
        if (request.allowNegativeStock() != null) {
            product.setAllowNegativeStock(request.allowNegativeStock());
        }
        if (request.imageUrl() != null) {
            product.setImageUrl(normalize(request.imageUrl()));
        }
        if (request.categoryId() != null) {
            product.setCategory(requireCategory(request.categoryId()));
        }

        Product saved = productRepo.save(product);
        return mapper.toProductDto(saved);
    }

    private Product requireProduct(Long id) {
        if (id == null) {
            throw new ApiBadRequestException("product id is required.");
        }
        return productRepo.findById(id)
                .orElseThrow(() -> new ApiNotFoundException("product not found."));
    }

    private Category requireCategory(Long id) {
        return categoryRepo.findById(id)
                .orElseThrow(() -> new ApiNotFoundException("category not found."));
    }

    private String normalize(String value) {
        if (value == null) {
            return null;
        }
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private String requireText(String value, String message) {
        String normalized = normalize(value);
        if (normalized == null) {
            throw new ApiBadRequestException(message);
        }
        return normalized;
    }

    private Specification<Product> buildSpecification(String q, Long categoryId, Boolean active, Boolean lowStock) {
        return (root, query, cb) -> {
            var predicates = new ArrayList<Predicate>();
            String text = normalize(q);
            if (text != null) {
                String like = "%" + text.toLowerCase(Locale.ROOT) + "%";
                predicates.add(
                        cb.or(
                                cb.like(cb.lower(root.get("name")), like),
                                cb.like(cb.lower(cb.coalesce(root.get("sku"), "")), like),
                                cb.like(cb.lower(cb.coalesce(root.get("barcode"), "")), like)
                        )
                );
            }
            if (categoryId != null) {
                predicates.add(cb.equal(root.get("category").get("id"), categoryId));
            }
            if (active != null) {
                predicates.add(cb.equal(root.get("active"), active));
            }
            if (Boolean.TRUE.equals(lowStock)) {
                predicates.add(cb.isNotNull(root.get("stockQty")));
                predicates.add(cb.isNotNull(root.get("lowStockThreshold")));
                predicates.add(cb.lessThanOrEqualTo(root.get("stockQty"), root.get("lowStockThreshold")));
            }
            return cb.and(predicates.toArray(Predicate[]::new));
        };
    }
}
