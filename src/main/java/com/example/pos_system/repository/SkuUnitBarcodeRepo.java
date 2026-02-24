package com.example.pos_system.repository;

import com.example.pos_system.entity.SkuSellUnit;
import com.example.pos_system.entity.SkuUnitBarcode;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface SkuUnitBarcodeRepo extends JpaRepository<SkuUnitBarcode, Long> {
    List<SkuUnitBarcode> findBySkuSellUnitOrderByIsPrimaryDescIdAsc(SkuSellUnit skuSellUnit);
    Optional<SkuUnitBarcode> findByBarcode(String barcode);
    Optional<SkuUnitBarcode> findByBarcodeIgnoreCase(String barcode);
}
