package com.example.pos_system.repository;

import com.example.pos_system.entity.ProductVariant;
import com.example.pos_system.entity.SkuSellUnit;
import com.example.pos_system.entity.UnitOfMeasure;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

public interface SkuSellUnitRepo extends JpaRepository<SkuSellUnit, Long> {
    List<SkuSellUnit> findByVariantOrderByIsBaseDescIdAsc(ProductVariant variant);
    List<SkuSellUnit> findByVariantAndEnabledTrueOrderByIsBaseDescIdAsc(ProductVariant variant);
    Optional<SkuSellUnit> findByVariantAndUnit(ProductVariant variant, UnitOfMeasure unit);
    Optional<SkuSellUnit> findByVariantAndId(ProductVariant variant, Long id);
    Optional<SkuSellUnit> findFirstByVariantAndEnabledTrueOrderByIsBaseDescIdAsc(ProductVariant variant);
    List<SkuSellUnit> findByIdIn(Collection<Long> ids);
}
