package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.SkuInventoryBalance;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;

public interface SkuInventoryBalanceRepo extends JpaRepository<SkuInventoryBalance, Long> {
    /**
     * Executes the findByVariantIdForUpdate operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<SkuInventoryBalance>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findByVariantIdForUpdate operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<SkuInventoryBalance>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findByVariantIdForUpdate operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<SkuInventoryBalance>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("select b from SkuInventoryBalance b where b.variantId = :variantId")
    Optional<SkuInventoryBalance> findByVariantIdForUpdate(@Param("variantId") Long variantId);
}
