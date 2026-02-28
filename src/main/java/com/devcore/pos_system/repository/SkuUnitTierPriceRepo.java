package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.SkuSellUnit;
import com.devcore.pos_system.entity.SkuUnitTierPrice;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;
import java.util.List;

public interface SkuUnitTierPriceRepo extends JpaRepository<SkuUnitTierPrice, Long> {
    /**
     * Executes the findBySkuSellUnitOrderByMinQtyDescIdDesc operation.
     *
     * @param skuSellUnit Parameter of type {@code SkuSellUnit} used by this operation.
     * @return {@code List<SkuUnitTierPrice>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<SkuUnitTierPrice> findBySkuSellUnitOrderByMinQtyDescIdDesc(SkuSellUnit skuSellUnit);
    /**
     * Executes the findBySkuSellUnitIn operation.
     *
     * @param sellUnits Parameter of type {@code Collection<SkuSellUnit>} used by this operation.
     * @return {@code List<SkuUnitTierPrice>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<SkuUnitTierPrice> findBySkuSellUnitIn(Collection<SkuSellUnit> sellUnits);
    /**
     * Executes the deleteBySkuSellUnit operation.
     *
     * @param skuSellUnit Parameter of type {@code SkuSellUnit} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    void deleteBySkuSellUnit(SkuSellUnit skuSellUnit);
}
