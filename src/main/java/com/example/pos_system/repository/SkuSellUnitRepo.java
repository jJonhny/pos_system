package com.example.pos_system.repository;

import com.example.pos_system.entity.ProductVariant;
import com.example.pos_system.entity.SkuSellUnit;
import com.example.pos_system.entity.UnitOfMeasure;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

public interface SkuSellUnitRepo extends JpaRepository<SkuSellUnit, Long> {
    /**
     * Executes the findByVariantOrderByIsBaseDescIdAsc operation.
     *
     * @param variant Parameter of type {@code ProductVariant} used by this operation.
     * @return {@code List<SkuSellUnit>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<SkuSellUnit> findByVariantOrderByIsBaseDescIdAsc(ProductVariant variant);
    /**
     * Executes the findByVariantAndEnabledTrueOrderByIsBaseDescIdAsc operation.
     *
     * @param variant Parameter of type {@code ProductVariant} used by this operation.
     * @return {@code List<SkuSellUnit>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<SkuSellUnit> findByVariantAndEnabledTrueOrderByIsBaseDescIdAsc(ProductVariant variant);
    /**
     * Executes the findByVariantAndUnit operation.
     *
     * @param variant Parameter of type {@code ProductVariant} used by this operation.
     * @param unit Parameter of type {@code UnitOfMeasure} used by this operation.
     * @return {@code Optional<SkuSellUnit>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<SkuSellUnit> findByVariantAndUnit(ProductVariant variant, UnitOfMeasure unit);
    /**
     * Executes the findByVariantAndId operation.
     *
     * @param variant Parameter of type {@code ProductVariant} used by this operation.
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<SkuSellUnit>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<SkuSellUnit> findByVariantAndId(ProductVariant variant, Long id);
    /**
     * Executes the findFirstByVariantAndEnabledTrueOrderByIsBaseDescIdAsc operation.
     *
     * @param variant Parameter of type {@code ProductVariant} used by this operation.
     * @return {@code Optional<SkuSellUnit>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<SkuSellUnit> findFirstByVariantAndEnabledTrueOrderByIsBaseDescIdAsc(ProductVariant variant);
    /**
     * Executes the findByIdIn operation.
     *
     * @param ids Parameter of type {@code Collection<Long>} used by this operation.
     * @return {@code List<SkuSellUnit>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<SkuSellUnit> findByIdIn(Collection<Long> ids);
}
