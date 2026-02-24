package com.example.pos_system.repository;

import com.example.pos_system.entity.SkuSellUnit;
import com.example.pos_system.entity.SkuUnitTierPrice;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;
import java.util.List;

public interface SkuUnitTierPriceRepo extends JpaRepository<SkuUnitTierPrice, Long> {
    List<SkuUnitTierPrice> findBySkuSellUnitOrderByMinQtyDescIdDesc(SkuSellUnit skuSellUnit);
    List<SkuUnitTierPrice> findBySkuSellUnitIn(Collection<SkuSellUnit> sellUnits);
    void deleteBySkuSellUnit(SkuSellUnit skuSellUnit);
}
