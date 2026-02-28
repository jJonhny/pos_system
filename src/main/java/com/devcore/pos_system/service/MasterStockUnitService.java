package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.UnitOfMeasure;
import com.devcore.pos_system.repository.SkuSellUnitRepo;
import com.devcore.pos_system.repository.UnitOfMeasureRepo;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

@Service
@Transactional
public class MasterStockUnitService {
    private final UnitOfMeasureRepo unitOfMeasureRepo;
    private final SkuSellUnitRepo skuSellUnitRepo;

    public MasterStockUnitService(UnitOfMeasureRepo unitOfMeasureRepo, SkuSellUnitRepo skuSellUnitRepo) {
        this.unitOfMeasureRepo = unitOfMeasureRepo;
        this.skuSellUnitRepo = skuSellUnitRepo;
    }

    @Transactional(readOnly = true)
    public List<UnitOfMeasure> listForManagement() {
        return unitOfMeasureRepo.findAllByOrderByActiveDescNameAscCodeAsc();
    }

    @Transactional(readOnly = true)
    public List<String> listActiveUnitNames() {
        return unitOfMeasureRepo.findByActiveTrueOrderByNameAscCodeAsc().stream()
                .map(UnitOfMeasure::getName)
                .filter(this::hasText)
                .map(String::trim)
                .distinct()
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public UnitOfMeasure require(Long id) {
        return unitOfMeasureRepo.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Master stock unit not found."));
    }

    public UnitOfMeasure create(String codeRaw, String nameRaw, Integer precisionScaleRaw, Boolean activeRaw) {
        String code = normalizeCode(codeRaw);
        String name = normalizeName(nameRaw);
        validateRequired(code, name);
        if (unitOfMeasureRepo.existsByCodeIgnoreCase(code)) {
            throw new IllegalArgumentException("MSW code already exists.");
        }
        UnitOfMeasure unit = new UnitOfMeasure();
        unit.setCode(code);
        unit.setName(name);
        unit.setPrecisionScale(normalizePrecision(precisionScaleRaw));
        unit.setActive(activeRaw == null || activeRaw);
        return unitOfMeasureRepo.save(unit);
    }

    public UnitOfMeasure update(Long id, String codeRaw, String nameRaw, Integer precisionScaleRaw, Boolean activeRaw) {
        UnitOfMeasure unit = require(id);
        String code = normalizeCode(codeRaw);
        String name = normalizeName(nameRaw);
        validateRequired(code, name);
        if (unitOfMeasureRepo.existsByCodeIgnoreCaseAndIdNot(code, id)) {
            throw new IllegalArgumentException("MSW code already exists.");
        }
        unit.setCode(code);
        unit.setName(name);
        unit.setPrecisionScale(normalizePrecision(precisionScaleRaw));
        unit.setActive(activeRaw == null || activeRaw);
        return unitOfMeasureRepo.save(unit);
    }

    public void delete(Long id) {
        UnitOfMeasure unit = require(id);
        if (skuSellUnitRepo.existsByUnit_Id(id)) {
            throw new IllegalStateException("Cannot delete MSW that is already used by variant sell units.");
        }
        unitOfMeasureRepo.delete(unit);
    }

    @Transactional(readOnly = true)
    public Map<Long, Long> usageCounts(Collection<Long> unitIds) {
        if (unitIds == null || unitIds.isEmpty()) return Collections.emptyMap();
        Map<Long, Long> map = new LinkedHashMap<>();
        skuSellUnitRepo.countUsageByUnitIds(unitIds).forEach(row -> {
            if (row.getUnitId() != null) {
                map.put(row.getUnitId(), row.getUsageCount() == null ? 0L : row.getUsageCount());
            }
        });
        return map;
    }

    private void validateRequired(String code, String name) {
        if (!hasText(code)) {
            throw new IllegalArgumentException("MSW code is required.");
        }
        if (!hasText(name)) {
            throw new IllegalArgumentException("MSW name is required.");
        }
    }

    private Integer normalizePrecision(Integer value) {
        if (value == null || value < 0) return 0;
        return value;
    }

    private String normalizeCode(String raw) {
        if (raw == null) return null;
        String trimmed = raw.trim();
        if (trimmed.isEmpty()) return null;
        return trimmed.toUpperCase(Locale.ROOT);
    }

    private String normalizeName(String raw) {
        if (raw == null) return null;
        String trimmed = raw.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private boolean hasText(String value) {
        return value != null && !value.trim().isEmpty();
    }
}
