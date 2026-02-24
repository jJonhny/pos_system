package com.example.pos_system.repository;

import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.ProductVariant;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

public interface ProductVariantRepo extends JpaRepository<ProductVariant, Long> {
    List<ProductVariant> findByProductAndArchivedFalseOrderByIdAsc(Product product);
    List<ProductVariant> findByProductAndArchivedFalseAndEnabledTrueAndImpossibleFalseOrderByIdAsc(Product product);
    List<ProductVariant> findByProductOrderByIdAsc(Product product);
    Optional<ProductVariant> findByProductAndCombinationHash(Product product, String combinationHash);
    List<ProductVariant> findByIdIn(Collection<Long> ids);
    Optional<ProductVariant> findByBarcode(String barcode);
    Optional<ProductVariant> findBySkuIgnoreCase(String sku);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("select v from ProductVariant v join fetch v.product where v.id = :id")
    Optional<ProductVariant> findByIdForUpdate(@Param("id") Long id);
}
