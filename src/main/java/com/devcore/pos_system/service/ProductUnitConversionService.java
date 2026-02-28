package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.ProductUnit;
import com.devcore.pos_system.repository.ProductRepo;
import com.devcore.pos_system.repository.ProductUnitRepo;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Comparator;
import java.util.List;

@Service
@Transactional(readOnly = true)
public class ProductUnitConversionService {
    private final ProductRepo productRepo;
    private final ProductUnitRepo productUnitRepo;
    private final boolean uomEnabled;

    public ProductUnitConversionService(ProductRepo productRepo,
                                        ProductUnitRepo productUnitRepo,
                                        @Value("${app.uom.product-units.enabled:true}") boolean uomEnabled) {
        this.productRepo = productRepo;
        this.productUnitRepo = productUnitRepo;
        this.uomEnabled = uomEnabled;
    }

    public boolean isUomEnabled() {
        return uomEnabled;
    }

    public BigDecimal toBase(Long productId, Long unitId, BigDecimal qty) {
        Product product = requireProduct(productId);
        return toBase(product, unitId, qty);
    }

    public BigDecimal toBase(Product product, Long unitId, BigDecimal qty) {
        BigDecimal safeQty = normalizeQty(qty);
        if (!uomEnabled || product == null) {
            return safeQty;
        }
        if (unitId == null) {
            return scaleBase(safeQty, product);
        }
        ProductUnit unit = requireUnit(product.getId(), unitId);
        return scaleBase(safeQty.multiply(unit.getConversionToBase()), product);
    }

    public BigDecimal fromBase(Long productId, Long unitId, BigDecimal qtyBase) {
        Product product = requireProduct(productId);
        return fromBase(product, unitId, qtyBase);
    }

    public BigDecimal fromBase(Product product, Long unitId, BigDecimal qtyBase) {
        BigDecimal safeBase = normalizeQty(qtyBase);
        if (!uomEnabled || product == null || unitId == null) {
            return scaleDisplay(safeBase);
        }
        ProductUnit unit = requireUnit(product.getId(), unitId);
        BigDecimal conversion = unit.getConversionToBase();
        if (conversion == null || conversion.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("Unit conversion must be positive.");
        }
        return scaleDisplay(safeBase.divide(conversion, 6, RoundingMode.HALF_UP));
    }

    public Integer toBaseInt(Product product, Long unitId, BigDecimal qty) {
        BigDecimal base = toBase(product, unitId, qty);
        return base.setScale(0, RoundingMode.HALF_UP).intValue();
    }

    public BigDecimal unitConversionToBase(Product product, Long unitId) {
        if (!uomEnabled || unitId == null || product == null) {
            return BigDecimal.ONE;
        }
        ProductUnit unit = requireUnit(product.getId(), unitId);
        return normalizeQty(unit.getConversionToBase());
    }

    public List<ProductUnit> listUnits(Long productId) {
        if (productId == null || !uomEnabled) {
            return List.of();
        }
        return productUnitRepo.findByProduct_IdOrderByConversionToBaseAscIdAsc(productId);
    }

    public ProductUnit resolveDefaultSaleUnit(Product product) {
        if (product == null || product.getId() == null || !uomEnabled) return null;
        return productUnitRepo.findFirstByProduct_IdAndIsDefaultSaleUnitTrue(product.getId())
                .orElseGet(() -> firstUnit(product.getId()));
    }

    public ProductUnit resolveDefaultPurchaseUnit(Product product) {
        if (product == null || product.getId() == null || !uomEnabled) return null;
        return productUnitRepo.findFirstByProduct_IdAndIsDefaultPurchaseUnitTrue(product.getId())
                .orElseGet(() -> firstUnit(product.getId()));
    }

    private ProductUnit firstUnit(Long productId) {
        List<ProductUnit> units = productUnitRepo.findByProduct_IdOrderByConversionToBaseAscIdAsc(productId);
        if (units.isEmpty()) return null;
        return units.stream()
                .min(Comparator.comparing(ProductUnit::getConversionToBase)
                        .thenComparing(ProductUnit::getId))
                .orElse(units.getFirst());
    }

    private Product requireProduct(Long productId) {
        if (productId == null) {
            throw new IllegalArgumentException("Product id is required.");
        }
        return productRepo.findById(productId)
                .orElseThrow(() -> new IllegalArgumentException("Product not found."));
    }

    private ProductUnit requireUnit(Long productId, Long unitId) {
        if (unitId == null) {
            throw new IllegalArgumentException("Unit id is required.");
        }
        return productUnitRepo.findByIdAndProduct_Id(unitId, productId)
                .orElseThrow(() -> new IllegalArgumentException("Unit does not belong to this product."));
    }

    private BigDecimal normalizeQty(BigDecimal value) {
        if (value == null) return BigDecimal.ZERO;
        return value.max(BigDecimal.ZERO);
    }

    private BigDecimal scaleBase(BigDecimal value, Product product) {
        int precision = product.getBaseUnitPrecision() == null ? 0 : Math.max(0, product.getBaseUnitPrecision());
        return value.setScale(precision, RoundingMode.HALF_UP);
    }

    private BigDecimal scaleDisplay(BigDecimal value) {
        return value.setScale(6, RoundingMode.HALF_UP).stripTrailingZeros();
    }
}
