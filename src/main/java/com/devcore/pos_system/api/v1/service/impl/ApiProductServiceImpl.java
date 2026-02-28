package com.devcore.pos_system.api.v1.service.impl;

import com.devcore.pos_system.api.v1.dto.product.ApiProductDto;
import com.devcore.pos_system.api.v1.dto.product.ApiProductUnitDto;
import com.devcore.pos_system.api.v1.dto.product.ProductCreateRequest;
import com.devcore.pos_system.api.v1.dto.product.ProductUnitUpsertRequest;
import com.devcore.pos_system.api.v1.dto.product.ProductUpdateRequest;
import com.devcore.pos_system.api.v1.exception.ApiBadRequestException;
import com.devcore.pos_system.api.v1.exception.ApiNotFoundException;
import com.devcore.pos_system.api.v1.mapper.ApiV1Mapper;
import com.devcore.pos_system.api.v1.service.ApiProductService;
import com.devcore.pos_system.entity.Category;
import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.ProductUnit;
import com.devcore.pos_system.repository.CategoryRepo;
import com.devcore.pos_system.repository.ProductRepo;
import com.devcore.pos_system.repository.ProductUnitRepo;
import com.devcore.pos_system.service.ProductUnitAdminService;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

@Service
@Transactional
public class ApiProductServiceImpl implements ApiProductService {
    private final ProductRepo productRepo;
    private final CategoryRepo categoryRepo;
    private final ProductUnitRepo productUnitRepo;
    private final ProductUnitAdminService productUnitAdminService;
    private final ApiV1Mapper mapper;

    public ApiProductServiceImpl(ProductRepo productRepo,
                                 CategoryRepo categoryRepo,
                                 ProductUnitRepo productUnitRepo,
                                 ProductUnitAdminService productUnitAdminService,
                                 ApiV1Mapper mapper) {
        this.productRepo = productRepo;
        this.categoryRepo = categoryRepo;
        this.productUnitRepo = productUnitRepo;
        this.productUnitAdminService = productUnitAdminService;
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
        product.setWholesalePrice(request.wholesalePrice());
        product.setWholesaleMinQty(request.wholesaleMinQty());
        product.setLowStockThreshold(request.lowStockThreshold() == null ? 0 : request.lowStockThreshold());
        product.setActive(request.active() == null || request.active());
        product.setAllowNegativeStock(request.allowNegativeStock() != null && request.allowNegativeStock());
        product.setImageUrl(normalize(request.imageUrl()));
        product.setBaseUnitName(normalize(request.baseUnitName()));
        product.setBaseUnitPrecision(request.baseUnitPrecision());
        product.setStockQty(0);

        if (request.categoryId() != null) {
            product.setCategory(requireCategory(request.categoryId()));
        }

        Product saved = productRepo.save(product);
        applySelectedUnitIds(saved, request.retailPriceUnitId(), request.wholesalePriceUnitId(),
                request.wholesaleMinQtyUnitId(), request.lowStockThresholdUnitId());
        return mapper.toProductDto(productRepo.save(saved));
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
        if (request.wholesalePrice() != null) {
            product.setWholesalePrice(request.wholesalePrice());
        }
        if (request.wholesaleMinQty() != null) {
            product.setWholesaleMinQty(request.wholesaleMinQty());
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
        if (request.baseUnitName() != null) {
            product.setBaseUnitName(normalize(request.baseUnitName()));
        }
        if (request.baseUnitPrecision() != null) {
            product.setBaseUnitPrecision(Math.max(0, request.baseUnitPrecision()));
        }
        if (request.categoryId() != null) {
            product.setCategory(requireCategory(request.categoryId()));
        }
        if (request.retailPriceUnitId() != null
                || request.wholesalePriceUnitId() != null
                || request.wholesaleMinQtyUnitId() != null
                || request.lowStockThresholdUnitId() != null) {
            applySelectedUnitIds(product, request.retailPriceUnitId(), request.wholesalePriceUnitId(),
                    request.wholesaleMinQtyUnitId(), request.lowStockThresholdUnitId());
        }

        Product saved = productRepo.save(product);
        return mapper.toProductDto(saved);
    }

    @Override
    @Transactional(readOnly = true)
    public List<ApiProductUnitDto> listUnits(Long productId) {
        Product product = requireProduct(productId);
        return productUnitRepo.findByProduct_IdOrderByConversionToBaseAscIdAsc(product.getId())
                .stream()
                .map(mapper::toProductUnitDto)
                .toList();
    }

    @Override
    public ApiProductUnitDto createUnit(Long productId, ProductUnitUpsertRequest request) {
        Product product = requireProduct(productId);
        ProductUnit unit = new ProductUnit();
        unit.setProduct(product);
        applyUnitRequest(unit, request);
        ProductUnit saved = productUnitRepo.save(unit);
        applyDefaultFlags(productId, saved.getId(), request.defaultSaleUnit(), request.defaultPurchaseUnit());
        productUnitAdminService.applyLegacyFallbackFields(product, productUnitRepo.findByProduct_IdOrderByConversionToBaseAscIdAsc(productId));
        productRepo.save(product);
        return mapper.toProductUnitDto(productUnitRepo.findById(saved.getId()).orElse(saved));
    }

    @Override
    public ApiProductUnitDto updateUnit(Long productId, Long unitId, ProductUnitUpsertRequest request) {
        requireProduct(productId);
        ProductUnit unit = productUnitRepo.findByIdAndProduct_Id(unitId, productId)
                .orElseThrow(() -> new ApiNotFoundException("product unit not found."));
        applyUnitRequest(unit, request);
        ProductUnit saved = productUnitRepo.save(unit);
        applyDefaultFlags(productId, saved.getId(), request.defaultSaleUnit(), request.defaultPurchaseUnit());
        Product product = requireProduct(productId);
        productUnitAdminService.applyLegacyFallbackFields(product, productUnitRepo.findByProduct_IdOrderByConversionToBaseAscIdAsc(productId));
        productRepo.save(product);
        return mapper.toProductUnitDto(productUnitRepo.findById(saved.getId()).orElse(saved));
    }

    @Override
    public void deleteUnit(Long productId, Long unitId) {
        Product product = requireProduct(productId);
        ProductUnit unit = productUnitRepo.findByIdAndProduct_Id(unitId, productId)
                .orElseThrow(() -> new ApiNotFoundException("product unit not found."));
        productUnitRepo.delete(unit);

        List<ProductUnit> remaining = productUnitRepo.findByProduct_IdOrderByConversionToBaseAscIdAsc(productId);
        ensureSingleDefaults(remaining);
        productUnitRepo.saveAll(remaining);

        if (product.getRetailPriceUnitId() != null && product.getRetailPriceUnitId().equals(unitId)) {
            product.setRetailPriceUnitId(null);
        }
        if (product.getWholesalePriceUnitId() != null && product.getWholesalePriceUnitId().equals(unitId)) {
            product.setWholesalePriceUnitId(null);
        }
        if (product.getWholesaleMinQtyUnitId() != null && product.getWholesaleMinQtyUnitId().equals(unitId)) {
            product.setWholesaleMinQtyUnitId(null);
        }
        if (product.getLowStockThresholdUnitId() != null && product.getLowStockThresholdUnitId().equals(unitId)) {
            product.setLowStockThresholdUnitId(null);
        }
        productUnitAdminService.applyLegacyFallbackFields(product, remaining);
        productRepo.save(product);
    }

    private void applyUnitRequest(ProductUnit unit, ProductUnitUpsertRequest request) {
        String name = requireText(request.name(), "unit name is required.");
        BigDecimal conversion = request.conversionToBase();
        if (conversion == null || conversion.compareTo(BigDecimal.ZERO) <= 0) {
            throw new ApiBadRequestException("conversionToBase must be greater than zero.");
        }
        unit.setName(name);
        unit.setAbbreviation(normalize(request.abbreviation()));
        unit.setConversionToBase(conversion);
        unit.setAllowForSale(request.allowForSale() == null || request.allowForSale());
        unit.setAllowForPurchase(request.allowForPurchase() == null || request.allowForPurchase());
        unit.setBarcode(normalize(request.barcode()));
    }

    private void applySelectedUnitIds(Product product,
                                      Long retailPriceUnitId,
                                      Long wholesalePriceUnitId,
                                      Long wholesaleMinQtyUnitId,
                                      Long lowStockThresholdUnitId) {
        product.setRetailPriceUnitId(assertUnitBelongs(product.getId(), retailPriceUnitId));
        product.setWholesalePriceUnitId(assertUnitBelongs(product.getId(), wholesalePriceUnitId));
        product.setWholesaleMinQtyUnitId(assertUnitBelongs(product.getId(), wholesaleMinQtyUnitId));
        product.setLowStockThresholdUnitId(assertUnitBelongs(product.getId(), lowStockThresholdUnitId));
    }

    private Long assertUnitBelongs(Long productId, Long unitId) {
        if (unitId == null) return null;
        if (!productUnitRepo.existsByIdAndProduct_Id(unitId, productId)) {
            throw new ApiBadRequestException("selected unit does not belong to this product.");
        }
        return unitId;
    }

    private void applyDefaultFlags(Long productId,
                                   Long targetUnitId,
                                   Boolean defaultSaleUnit,
                                   Boolean defaultPurchaseUnit) {
        List<ProductUnit> units = productUnitRepo.findByProduct_IdOrderByConversionToBaseAscIdAsc(productId);
        if (units.isEmpty()) return;
        if (Boolean.TRUE.equals(defaultSaleUnit)) {
            for (ProductUnit unit : units) {
                unit.setIsDefaultSaleUnit(unit.getId() != null && unit.getId().equals(targetUnitId));
            }
        }
        if (Boolean.TRUE.equals(defaultPurchaseUnit)) {
            for (ProductUnit unit : units) {
                unit.setIsDefaultPurchaseUnit(unit.getId() != null && unit.getId().equals(targetUnitId));
            }
        }
        ensureSingleDefaults(units);
        productUnitRepo.saveAll(units);
    }

    private void ensureSingleDefaults(List<ProductUnit> units) {
        if (units == null || units.isEmpty()) return;
        int saleDefaults = 0;
        int purchaseDefaults = 0;
        for (ProductUnit unit : units) {
            if (Boolean.TRUE.equals(unit.getIsDefaultSaleUnit())) saleDefaults++;
            if (Boolean.TRUE.equals(unit.getIsDefaultPurchaseUnit())) purchaseDefaults++;
        }
        if (saleDefaults != 1) {
            for (ProductUnit unit : units) {
                unit.setIsDefaultSaleUnit(false);
            }
            ProductUnit saleDefault = units.stream()
                    .filter(u -> Boolean.TRUE.equals(u.getAllowForSale()))
                    .findFirst()
                    .orElse(units.getFirst());
            saleDefault.setAllowForSale(true);
            saleDefault.setIsDefaultSaleUnit(true);
        }
        if (purchaseDefaults != 1) {
            for (ProductUnit unit : units) {
                unit.setIsDefaultPurchaseUnit(false);
            }
            ProductUnit purchaseDefault = units.stream()
                    .filter(u -> Boolean.TRUE.equals(u.getAllowForPurchase()))
                    .findFirst()
                    .orElse(units.getFirst());
            purchaseDefault.setAllowForPurchase(true);
            purchaseDefault.setIsDefaultPurchaseUnit(true);
        }
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
