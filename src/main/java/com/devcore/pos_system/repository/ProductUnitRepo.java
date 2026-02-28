package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.ProductUnit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

public interface ProductUnitRepo extends JpaRepository<ProductUnit, Long> {
    List<ProductUnit> findByProduct_IdOrderByConversionToBaseAscIdAsc(Long productId);

    Optional<ProductUnit> findByIdAndProduct_Id(Long id, Long productId);

    Optional<ProductUnit> findFirstByProduct_IdAndIsDefaultSaleUnitTrue(Long productId);

    Optional<ProductUnit> findFirstByProduct_IdAndIsDefaultPurchaseUnitTrue(Long productId);

    List<ProductUnit> findByProduct_IdIn(Collection<Long> productIds);

    void deleteByProduct_Id(Long productId);

    void deleteByProduct_IdAndIdNotIn(Long productId, Collection<Long> ids);

    boolean existsByIdAndProduct_Id(Long id, Long productId);

    boolean existsByBarcodeIgnoreCaseAndProduct_IdNot(String barcode, Long productId);
}
