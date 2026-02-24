package com.example.pos_system.repository;

import com.example.pos_system.entity.SkuInventoryBalance;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;

public interface SkuInventoryBalanceRepo extends JpaRepository<SkuInventoryBalance, Long> {
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("select b from SkuInventoryBalance b where b.variantId = :variantId")
    Optional<SkuInventoryBalance> findByVariantIdForUpdate(@Param("variantId") Long variantId);
}
