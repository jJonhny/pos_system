package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.ProductUnit;
import com.devcore.pos_system.repository.ProductUnitRepo;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

@Service
@Transactional
public class ProductUnitAdminService {
    private static final int MAX_UNIT_NAME_LENGTH = 64;
    private static final int MAX_UNIT_ABBREV_LENGTH = 32;
    private static final int MAX_UNIT_BARCODE_LENGTH = 128;
    private static final BigDecimal MIN_CONVERSION_TO_BASE = new BigDecimal("0.0001");
    private static final BigDecimal MAX_CONVERSION_TO_BASE = new BigDecimal("1000000");

    private final ProductUnitRepo productUnitRepo;
    private final boolean globalBarcodeUnique;

    public ProductUnitAdminService(ProductUnitRepo productUnitRepo,
                                   @Value("${app.uom.product-units.global-barcode-unique:false}") boolean globalBarcodeUnique) {
        this.productUnitRepo = productUnitRepo;
        this.globalBarcodeUnique = globalBarcodeUnique;
    }

    @Transactional(readOnly = true)
    public List<ProductUnit> listProductUnits(Long productId) {
        if (productId == null) {
            return List.of();
        }
        return productUnitRepo.findByProduct_IdOrderByConversionToBaseAscIdAsc(productId);
    }

    @Transactional(readOnly = true)
    public Optional<ProductUnit> findUnit(Long productId, Long unitId) {
        if (productId == null || unitId == null) {
            return Optional.empty();
        }
        return productUnitRepo.findByIdAndProduct_Id(unitId, productId);
    }

    @Transactional(readOnly = true)
    public boolean belongsToProduct(Long productId, Long unitId) {
        if (productId == null || unitId == null) {
            return false;
        }
        return productUnitRepo.existsByIdAndProduct_Id(unitId, productId);
    }

    public List<ProductUnit> replaceUnits(Product product,
                                          List<ProductUnitDraft> drafts,
                                          Integer defaultSaleIndex,
                                          Integer defaultPurchaseIndex) {
        if (product == null || product.getId() == null) {
            throw new IllegalArgumentException("Product must be saved before managing product units.");
        }

        List<ProductUnitDraft> safeDrafts = drafts == null ? List.of() : drafts;
        List<ProductUnit> existing = productUnitRepo.findByProduct_IdOrderByConversionToBaseAscIdAsc(product.getId());
        Map<Long, ProductUnit> existingById = new HashMap<>();
        for (ProductUnit unit : existing) {
            if (unit.getId() != null) {
                existingById.put(unit.getId(), unit);
            }
        }

        Set<String> seenNames = new HashSet<>();
        Set<String> seenBarcodes = new HashSet<>();
        String baseUnitName = trimToNull(product.getBaseUnitName());
        List<ProductUnit> normalized = new ArrayList<>();
        for (int i = 0; i < safeDrafts.size(); i++) {
            ProductUnitDraft draft = safeDrafts.get(i);
            if (draft == null) continue;
            String name = trimToNull(draft.name());
            if (name == null) {
                throw new ProductUnitValidationException("Packaging unit name is required.");
            }
            if (name.length() > MAX_UNIT_NAME_LENGTH) {
                throw new ProductUnitValidationException("Packaging unit name exceeds " + MAX_UNIT_NAME_LENGTH + " characters.");
            }
            BigDecimal conversion = normalizeConversion(draft.conversionToBase());
            if (conversion.compareTo(MIN_CONVERSION_TO_BASE) < 0
                    || conversion.compareTo(MAX_CONVERSION_TO_BASE) > 0) {
                throw new ProductUnitValidationException("Packaging conversion must be between 0.0001 and 1,000,000.");
            }

            String abbreviation = trimToNull(draft.abbreviation());
            if (abbreviation != null && abbreviation.length() > MAX_UNIT_ABBREV_LENGTH) {
                throw new ProductUnitValidationException("Packaging abbreviation exceeds " + MAX_UNIT_ABBREV_LENGTH + " characters.");
            }
            if (abbreviation != null && baseUnitName != null && abbreviation.equalsIgnoreCase(baseUnitName)) {
                throw new ProductUnitValidationException("Packaging abbreviation must not equal the base unit name.");
            }

            String barcode = trimToNull(draft.barcode());
            if (barcode != null && barcode.length() > MAX_UNIT_BARCODE_LENGTH) {
                throw new ProductUnitValidationException("Packaging barcode exceeds " + MAX_UNIT_BARCODE_LENGTH + " characters.");
            }

            String nameKey = name.toLowerCase(Locale.ROOT);
            if (!seenNames.add(nameKey)) {
                throw new ProductUnitValidationException("Duplicate packaging unit name: " + name);
            }
            if (barcode != null) {
                String barcodeKey = barcode.toLowerCase(Locale.ROOT);
                if (!seenBarcodes.add(barcodeKey)) {
                    throw new ProductUnitValidationException("Duplicate packaging barcode in this product: " + barcode);
                }
                if (globalBarcodeUnique && productUnitRepo.existsByBarcodeIgnoreCaseAndProduct_IdNot(barcode, product.getId())) {
                    throw new ProductUnitValidationException("Packaging barcode is already used by another product unit.");
                }
            }
            ProductUnit target = null;
            if (draft.id() != null) {
                target = existingById.get(draft.id());
                if (target == null) {
                    throw new ProductUnitValidationException("Packaging unit row references an unknown unit id.");
                }
            }
            if (target == null) {
                target = new ProductUnit();
                target.setProduct(product);
            }
            target.setName(name);
            target.setAbbreviation(abbreviation);
            target.setConversionToBase(conversion);
            target.setAllowForSale(Boolean.TRUE.equals(draft.allowForSale()));
            target.setAllowForPurchase(Boolean.TRUE.equals(draft.allowForPurchase()));
            target.setBarcode(barcode);
            target.setIsDefaultSaleUnit(false);
            target.setIsDefaultPurchaseUnit(false);
            normalized.add(target);
        }

        if (normalized.isEmpty()) {
            productUnitRepo.deleteByProduct_Id(product.getId());
            return List.of();
        }

        applyDefaultFlags(normalized, defaultSaleIndex, defaultPurchaseIndex);
        List<ProductUnit> saved = productUnitRepo.saveAll(normalized);

        List<Long> keepIds = saved.stream()
                .map(ProductUnit::getId)
                .filter(Objects::nonNull)
                .toList();
        if (keepIds.isEmpty()) {
            productUnitRepo.deleteByProduct_Id(product.getId());
        } else {
            productUnitRepo.deleteByProduct_IdAndIdNotIn(product.getId(), keepIds);
        }

        return productUnitRepo.findByProduct_IdOrderByConversionToBaseAscIdAsc(product.getId());
    }

    public void applyLegacyFallbackFields(Product product, Collection<ProductUnit> productUnits) {
        if (product == null) return;
        if (productUnits == null || productUnits.isEmpty()) {
            return;
        }
        List<Integer> conversions = productUnits.stream()
                .filter(Objects::nonNull)
                .map(ProductUnit::getConversionToBase)
                .filter(Objects::nonNull)
                .filter(v -> v.compareTo(BigDecimal.ZERO) > 0)
                .sorted(Comparator.naturalOrder())
                .map(this::toPositiveInt)
                .filter(Objects::nonNull)
                .toList();

        Integer unitsPerBox = conversions.isEmpty() ? null : conversions.getFirst();
        Integer unitsPerCase = conversions.isEmpty() ? null : conversions.getLast();
        if (Objects.equals(unitsPerBox, unitsPerCase)) {
            unitsPerCase = null;
        }
        product.setUnitsPerBox(unitsPerBox);
        product.setUnitsPerCase(unitsPerCase);
    }

    @Transactional(readOnly = true)
    public Map<Long, BigDecimal> conversionMap(Long productId) {
        Map<Long, BigDecimal> map = new LinkedHashMap<>();
        if (productId == null) return map;
        List<ProductUnit> units = productUnitRepo.findByProduct_IdOrderByConversionToBaseAscIdAsc(productId);
        for (ProductUnit unit : units) {
            if (unit.getId() == null || unit.getConversionToBase() == null) continue;
            if (unit.getConversionToBase().compareTo(BigDecimal.ZERO) <= 0) continue;
            map.put(unit.getId(), unit.getConversionToBase());
        }
        return map;
    }

    private void applyDefaultFlags(List<ProductUnit> units,
                                   Integer defaultSaleIndex,
                                   Integer defaultPurchaseIndex) {
        int saleIndex = resolveDefaultIndex(units, defaultSaleIndex, true);
        int purchaseIndex = resolveDefaultIndex(units, defaultPurchaseIndex, false);

        for (int i = 0; i < units.size(); i++) {
            ProductUnit unit = units.get(i);
            unit.setIsDefaultSaleUnit(i == saleIndex);
            unit.setIsDefaultPurchaseUnit(i == purchaseIndex);
        }
    }

    private int resolveDefaultIndex(List<ProductUnit> units, Integer requestedIndex, boolean sale) {
        if (requestedIndex != null && requestedIndex >= 0 && requestedIndex < units.size()) {
            ProductUnit selected = units.get(requestedIndex);
            if ((sale && Boolean.TRUE.equals(selected.getAllowForSale()))
                    || (!sale && Boolean.TRUE.equals(selected.getAllowForPurchase()))) {
                return requestedIndex;
            }
            throw new ProductUnitValidationException(sale
                    ? "Default sale unit must be marked as sellable."
                    : "Default purchase unit must be marked as purchasable.");
        }
        if (requestedIndex != null) {
            throw new ProductUnitValidationException(sale
                    ? "Default sale unit selection is invalid."
                    : "Default purchase unit selection is invalid.");
        }
        for (int i = 0; i < units.size(); i++) {
            ProductUnit unit = units.get(i);
            if (sale && Boolean.TRUE.equals(unit.getAllowForSale())) {
                return i;
            }
            if (!sale && Boolean.TRUE.equals(unit.getAllowForPurchase())) {
                return i;
            }
        }
        return -1;
    }

    private BigDecimal normalizeConversion(BigDecimal value) {
        if (value == null) {
            throw new ProductUnitValidationException("Packaging conversion is required.");
        }
        if (value.compareTo(BigDecimal.ZERO) <= 0) {
            throw new ProductUnitValidationException("Packaging conversion must be greater than zero.");
        }
        return value.stripTrailingZeros();
    }

    private Integer toPositiveInt(BigDecimal value) {
        if (value == null) return null;
        try {
            int intValue = value.intValueExact();
            return intValue > 0 ? intValue : null;
        } catch (ArithmeticException ex) {
            return null;
        }
    }

    private String trimToNull(String value) {
        if (value == null) return null;
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    public record ProductUnitDraft(
            Long id,
            String name,
            String abbreviation,
            BigDecimal conversionToBase,
            Boolean allowForSale,
            Boolean allowForPurchase,
            String barcode
    ) {
    }

    public static class ProductUnitValidationException extends IllegalArgumentException {
        public ProductUnitValidationException(String message) {
            super(message);
        }
    }
}
