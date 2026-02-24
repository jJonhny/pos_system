package com.example.pos_system.service;

import com.example.pos_system.dto.VariantApiDtos;
import com.example.pos_system.entity.CustomerGroup;
import com.example.pos_system.entity.ProductVariant;
import com.example.pos_system.entity.SkuSellUnit;
import com.example.pos_system.entity.SkuUnitBarcode;
import com.example.pos_system.entity.SkuUnitTierPrice;
import com.example.pos_system.entity.UnitOfMeasure;
import com.example.pos_system.repository.CustomerGroupRepo;
import com.example.pos_system.repository.ProductVariantRepo;
import com.example.pos_system.repository.SkuSellUnitRepo;
import com.example.pos_system.repository.SkuUnitBarcodeRepo;
import com.example.pos_system.repository.SkuUnitTierPriceRepo;
import com.example.pos_system.repository.UnitOfMeasureRepo;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Transactional
public class SkuUnitPricingService {
    private final UnitOfMeasureRepo unitOfMeasureRepo;
    private final CustomerGroupRepo customerGroupRepo;
    private final ProductVariantRepo productVariantRepo;
    private final SkuSellUnitRepo skuSellUnitRepo;
    private final SkuUnitBarcodeRepo skuUnitBarcodeRepo;
    private final SkuUnitTierPriceRepo skuUnitTierPriceRepo;

    public SkuUnitPricingService(UnitOfMeasureRepo unitOfMeasureRepo,
                                 CustomerGroupRepo customerGroupRepo,
                                 ProductVariantRepo productVariantRepo,
                                 SkuSellUnitRepo skuSellUnitRepo,
                                 SkuUnitBarcodeRepo skuUnitBarcodeRepo,
                                 SkuUnitTierPriceRepo skuUnitTierPriceRepo) {
        this.unitOfMeasureRepo = unitOfMeasureRepo;
        this.customerGroupRepo = customerGroupRepo;
        this.productVariantRepo = productVariantRepo;
        this.skuSellUnitRepo = skuSellUnitRepo;
        this.skuUnitBarcodeRepo = skuUnitBarcodeRepo;
        this.skuUnitTierPriceRepo = skuUnitTierPriceRepo;
    }

    public UnitOfMeasure createUnit(VariantApiDtos.UnitCreateRequest request) {
        String code = normalizeCode(request == null ? null : request.code());
        String name = normalizeText(request == null ? null : request.name());
        if (code == null || name == null) {
            throw new IllegalArgumentException("Unit code and name are required.");
        }
        if (unitOfMeasureRepo.findByCodeIgnoreCase(code).isPresent()) {
            throw new IllegalArgumentException("Unit code already exists.");
        }
        UnitOfMeasure unit = new UnitOfMeasure();
        unit.setCode(code);
        unit.setName(name);
        unit.setPrecisionScale(request != null && request.precisionScale() != null ? request.precisionScale() : 0);
        unit.setActive(true);
        return unitOfMeasureRepo.save(unit);
    }

    public CustomerGroup createCustomerGroup(VariantApiDtos.CustomerGroupCreateRequest request) {
        String code = normalizeCode(request == null ? null : request.code());
        String name = normalizeText(request == null ? null : request.name());
        if (code == null || name == null) {
            throw new IllegalArgumentException("Customer group code and name are required.");
        }
        if (customerGroupRepo.findByCodeIgnoreCase(code).isPresent()) {
            throw new IllegalArgumentException("Customer group code already exists.");
        }
        CustomerGroup group = new CustomerGroup();
        group.setCode(code);
        group.setName(name);
        group.setPriority(request != null && request.priority() != null ? request.priority() : 0);
        group.setActive(true);
        return customerGroupRepo.save(group);
    }

    public SkuSellUnit upsertSellUnit(Long variantId, VariantApiDtos.SellUnitUpsertRequest request) {
        ProductVariant variant = requireVariant(variantId);
        UnitOfMeasure unit = requireUnitByCode(request == null ? null : request.unitCode());

        SkuSellUnit sellUnit = skuSellUnitRepo.findByVariantAndUnit(variant, unit).orElseGet(SkuSellUnit::new);
        sellUnit.setVariant(variant);
        sellUnit.setUnit(unit);
        applySellUnitValues(sellUnit, request);
        SkuSellUnit saved = skuSellUnitRepo.save(sellUnit);
        enforceSingleBase(saved);
        return saved;
    }

    public SkuSellUnit upsertSellUnitById(Long sellUnitId, VariantApiDtos.SellUnitUpsertRequest request) {
        SkuSellUnit sellUnit = skuSellUnitRepo.findById(sellUnitId)
                .orElseThrow(() -> new IllegalArgumentException("Sell unit not found."));

        if (request != null && request.unitCode() != null && !request.unitCode().isBlank()) {
            UnitOfMeasure unit = requireUnitByCode(request.unitCode());
            sellUnit.setUnit(unit);
        }
        applySellUnitValues(sellUnit, request);
        SkuSellUnit saved = skuSellUnitRepo.save(sellUnit);
        enforceSingleBase(saved);
        return saved;
    }

    public SkuUnitBarcode addBarcode(Long sellUnitId, VariantApiDtos.BarcodeCreateRequest request) {
        SkuSellUnit sellUnit = skuSellUnitRepo.findById(sellUnitId)
                .orElseThrow(() -> new IllegalArgumentException("Sell unit not found."));
        String barcode = normalizeText(request == null ? null : request.barcode());
        if (barcode == null) {
            throw new IllegalArgumentException("Barcode is required.");
        }
        skuUnitBarcodeRepo.findByBarcode(barcode).ifPresent(existing -> {
            throw new IllegalArgumentException("Barcode already exists.");
        });

        boolean isPrimary = request == null || request.isPrimary() == null || request.isPrimary();
        if (isPrimary) {
            List<SkuUnitBarcode> existingForUnit = skuUnitBarcodeRepo.findBySkuSellUnitOrderByIsPrimaryDescIdAsc(sellUnit);
            for (SkuUnitBarcode row : existingForUnit) {
                row.setIsPrimary(false);
                skuUnitBarcodeRepo.save(row);
            }
        }

        SkuUnitBarcode row = new SkuUnitBarcode();
        row.setSkuSellUnit(sellUnit);
        row.setBarcode(barcode);
        row.setIsPrimary(isPrimary);
        row.setActive(true);
        return skuUnitBarcodeRepo.save(row);
    }

    public List<SkuUnitTierPrice> replaceTierPrices(Long sellUnitId, VariantApiDtos.TierPriceReplaceRequest request) {
        SkuSellUnit sellUnit = skuSellUnitRepo.findById(sellUnitId)
                .orElseThrow(() -> new IllegalArgumentException("Sell unit not found."));
        List<VariantApiDtos.TierPriceItem> tiers = request == null || request.tiers() == null ? List.of() : request.tiers();

        skuUnitTierPriceRepo.deleteBySkuSellUnit(sellUnit);
        List<SkuUnitTierPrice> saved = new ArrayList<>();
        for (VariantApiDtos.TierPriceItem item : tiers) {
            if (item == null) continue;
            if (item.minQty() == null || item.minQty().compareTo(BigDecimal.ZERO) <= 0) {
                throw new IllegalArgumentException("minQty must be positive.");
            }
            if (item.unitPrice() == null || item.unitPrice().compareTo(BigDecimal.ZERO) < 0) {
                throw new IllegalArgumentException("unitPrice must be zero or positive.");
            }
            CustomerGroup customerGroup = null;
            if (item.customerGroupCode() != null && !item.customerGroupCode().isBlank()) {
                customerGroup = customerGroupRepo.findByCodeIgnoreCase(item.customerGroupCode().trim())
                        .orElseThrow(() -> new IllegalArgumentException("Unknown customer group code: " + item.customerGroupCode()));
            }
            SkuUnitTierPrice tier = new SkuUnitTierPrice();
            tier.setSkuSellUnit(sellUnit);
            tier.setCustomerGroup(customerGroup);
            tier.setMinQty(item.minQty());
            tier.setUnitPrice(item.unitPrice());
            String currencyCode = item.currencyCode();
            if (currencyCode == null || currencyCode.isBlank()) {
                currencyCode = request == null ? null : request.currencyCode();
            }
            tier.setCurrencyCode((currencyCode == null || currencyCode.isBlank()) ? "USD" : currencyCode.trim().toUpperCase(Locale.ROOT));
            tier.setEffectiveFrom(item.effectiveFrom());
            tier.setEffectiveTo(item.effectiveTo());
            tier.setActive(item.active() == null || item.active());
            saved.add(skuUnitTierPriceRepo.save(tier));
        }
        return saved;
    }

    @Transactional(readOnly = true)
    public VariantApiDtos.PricingQuoteResponse quote(VariantApiDtos.PricingQuoteRequest request) {
        List<VariantApiDtos.PricingQuoteLineRequest> lines = request == null || request.lines() == null
                ? List.of() : request.lines();
        if (lines.isEmpty()) {
            throw new IllegalArgumentException("At least one line is required.");
        }

        Map<Integer, ResolvedLine> resolvedLines = new LinkedHashMap<>();
        Set<Long> sellUnitIds = new java.util.LinkedHashSet<>();

        for (int i = 0; i < lines.size(); i++) {
            VariantApiDtos.PricingQuoteLineRequest line = lines.get(i);
            BigDecimal qty = line == null || line.qty() == null ? BigDecimal.ZERO : line.qty();
            if (qty.compareTo(BigDecimal.ZERO) <= 0) {
                throw new IllegalArgumentException("qty must be positive for all lines.");
            }
            SkuSellUnit sellUnit = resolveSellUnit(line);
            sellUnitIds.add(sellUnit.getId());
            resolvedLines.put(i, new ResolvedLine(line, sellUnit));
        }

        List<SkuSellUnit> sellUnits = skuSellUnitRepo.findByIdIn(sellUnitIds);
        Map<Long, SkuSellUnit> sellUnitMap = sellUnits.stream().collect(Collectors.toMap(SkuSellUnit::getId, s -> s));

        Map<Long, BigDecimal> aggregatedQtyBySellUnit = new HashMap<>();
        for (ResolvedLine line : resolvedLines.values()) {
            Long sellUnitId = line.sellUnit().getId();
            BigDecimal qty = line.request().qty();
            aggregatedQtyBySellUnit.merge(sellUnitId, qty, BigDecimal::add);
        }

        List<SkuUnitTierPrice> allTiers = skuUnitTierPriceRepo.findBySkuSellUnitIn(sellUnits);
        Map<Long, List<SkuUnitTierPrice>> tiersBySellUnit = allTiers.stream()
                .filter(t -> Boolean.TRUE.equals(t.getActive()))
                .collect(Collectors.groupingBy(t -> t.getSkuSellUnit().getId()));

        CustomerGroup customerGroup = null;
        if (request.customerGroupCode() != null && !request.customerGroupCode().isBlank()) {
            customerGroup = customerGroupRepo.findByCodeIgnoreCase(request.customerGroupCode().trim()).orElse(null);
        }

        String currency = request.currencyCode();
        if (currency == null || currency.isBlank()) {
            currency = "USD";
        }
        currency = currency.toUpperCase(Locale.ROOT);

        BigDecimal subtotal = BigDecimal.ZERO;
        List<VariantApiDtos.PricingQuoteLineResponse> responses = new ArrayList<>();
        LocalDateTime now = LocalDateTime.now();

        for (int i = 0; i < lines.size(); i++) {
            ResolvedLine resolved = resolvedLines.get(i);
            SkuSellUnit sellUnit = sellUnitMap.get(resolved.sellUnit().getId());
            BigDecimal qty = resolved.request().qty();
            BigDecimal qtyForTier = aggregatedQtyBySellUnit.getOrDefault(sellUnit.getId(), qty);
            List<SkuUnitTierPrice> candidateTiers = tiersBySellUnit.getOrDefault(sellUnit.getId(), List.of());

            SkuUnitTierPrice groupTier = bestTier(candidateTiers, customerGroup, qtyForTier, now);
            SkuUnitTierPrice globalTier = bestTier(candidateTiers, null, qtyForTier, now);

            BigDecimal unitPrice;
            String source;
            VariantApiDtos.AppliedTier appliedTier = null;
            if (groupTier != null) {
                unitPrice = groupTier.getUnitPrice();
                source = "GROUP_TIER";
                appliedTier = new VariantApiDtos.AppliedTier(
                        groupTier.getCustomerGroup() == null ? null : groupTier.getCustomerGroup().getCode(),
                        groupTier.getMinQty()
                );
            } else if (globalTier != null) {
                unitPrice = globalTier.getUnitPrice();
                source = "GLOBAL_TIER";
                appliedTier = new VariantApiDtos.AppliedTier(null, globalTier.getMinQty());
            } else {
                unitPrice = sellUnit.getBasePrice();
                source = "BASE_UNIT_PRICE";
            }

            BigDecimal lineSubtotal = unitPrice.multiply(qty).setScale(2, RoundingMode.HALF_UP);
            BigDecimal inventoryBaseQty = qty.multiply(sellUnit.getConversionToBase()).setScale(6, RoundingMode.HALF_UP);
            subtotal = subtotal.add(lineSubtotal);

            responses.add(new VariantApiDtos.PricingQuoteLineResponse(
                    sellUnit.getVariant().getId(),
                    sellUnit.getId(),
                    qty,
                    source,
                    appliedTier,
                    unitPrice,
                    lineSubtotal,
                    inventoryBaseQty
            ));
        }

        return new VariantApiDtos.PricingQuoteResponse(
                responses,
                subtotal.setScale(2, RoundingMode.HALF_UP),
                currency
        );
    }

    private SkuUnitTierPrice bestTier(List<SkuUnitTierPrice> tiers,
                                      CustomerGroup customerGroup,
                                      BigDecimal qty,
                                      LocalDateTime now) {
        if (tiers == null || tiers.isEmpty()) return null;
        return tiers.stream()
                .filter(t -> groupMatches(t, customerGroup))
                .filter(t -> qty.compareTo(t.getMinQty()) >= 0)
                .filter(t -> withinWindow(t, now))
                .max(Comparator.comparing(SkuUnitTierPrice::getMinQty)
                        .thenComparing(SkuUnitTierPrice::getId))
                .orElse(null);
    }

    private boolean groupMatches(SkuUnitTierPrice tier, CustomerGroup group) {
        if (group == null) {
            return tier.getCustomerGroup() == null;
        }
        if (tier.getCustomerGroup() == null) {
            return false;
        }
        return tier.getCustomerGroup().getId().equals(group.getId());
    }

    private boolean withinWindow(SkuUnitTierPrice tier, LocalDateTime now) {
        LocalDateTime from = tier.getEffectiveFrom();
        LocalDateTime to = tier.getEffectiveTo();
        if (from != null && now.isBefore(from)) return false;
        if (to != null && now.isAfter(to)) return false;
        return true;
    }

    private void applySellUnitValues(SkuSellUnit sellUnit, VariantApiDtos.SellUnitUpsertRequest request) {
        if (request == null) {
            throw new IllegalArgumentException("Sell unit payload is required.");
        }
        if (request.conversionToBase() == null || request.conversionToBase().compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("conversionToBase must be positive.");
        }
        if (request.basePrice() == null || request.basePrice().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("basePrice must be zero or positive.");
        }
        boolean isBase = request.isBase() != null && request.isBase();
        if (isBase && request.conversionToBase().compareTo(BigDecimal.ONE) != 0) {
            throw new IllegalArgumentException("Base unit conversionToBase must be 1.");
        }

        sellUnit.setConversionToBase(request.conversionToBase());
        sellUnit.setIsBase(isBase);
        sellUnit.setBasePrice(request.basePrice());
        sellUnit.setEnabled(request.enabled() == null || request.enabled());
    }

    private void enforceSingleBase(SkuSellUnit saved) {
        if (!Boolean.TRUE.equals(saved.getIsBase())) {
            return;
        }
        List<SkuSellUnit> all = skuSellUnitRepo.findByVariantOrderByIsBaseDescIdAsc(saved.getVariant());
        for (SkuSellUnit row : all) {
            if (row.getId().equals(saved.getId())) continue;
            if (Boolean.TRUE.equals(row.getIsBase())) {
                row.setIsBase(false);
                skuSellUnitRepo.save(row);
            }
        }
    }

    private SkuSellUnit resolveSellUnit(VariantApiDtos.PricingQuoteLineRequest line) {
        if (line == null) {
            throw new IllegalArgumentException("Line is required.");
        }
        if (line.sellUnitId() != null) {
            return skuSellUnitRepo.findById(line.sellUnitId())
                    .orElseThrow(() -> new IllegalArgumentException("Sell unit not found: " + line.sellUnitId()));
        }
        if (line.variantId() == null) {
            throw new IllegalArgumentException("Either sellUnitId or variantId is required.");
        }
        ProductVariant variant = requireVariant(line.variantId());
        return skuSellUnitRepo.findByVariantOrderByIsBaseDescIdAsc(variant).stream()
                .filter(v -> Boolean.TRUE.equals(v.getEnabled()))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("No sell unit configured for variant: " + line.variantId()));
    }

    private ProductVariant requireVariant(Long variantId) {
        return productVariantRepo.findById(variantId)
                .orElseThrow(() -> new IllegalArgumentException("Variant not found."));
    }

    private UnitOfMeasure requireUnitByCode(String code) {
        String normalized = normalizeCode(code);
        if (normalized == null) {
            throw new IllegalArgumentException("unitCode is required.");
        }
        return unitOfMeasureRepo.findByCodeIgnoreCase(normalized)
                .orElseThrow(() -> new IllegalArgumentException("Unit code not found: " + normalized));
    }

    private String normalizeCode(String value) {
        if (value == null) return null;
        String normalized = value.trim().toUpperCase(Locale.ROOT);
        return normalized.isEmpty() ? null : normalized;
    }

    private String normalizeText(String value) {
        if (value == null) return null;
        String normalized = value.trim();
        return normalized.isEmpty() ? null : normalized;
    }

    private record ResolvedLine(VariantApiDtos.PricingQuoteLineRequest request, SkuSellUnit sellUnit) {}
}
