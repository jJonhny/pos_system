package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.SkuSellUnit;
import com.devcore.pos_system.entity.SkuUnitBarcode;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface SkuUnitBarcodeRepo extends JpaRepository<SkuUnitBarcode, Long> {
    /**
     * Executes the findBySkuSellUnitOrderByIsPrimaryDescIdAsc operation.
     *
     * @param skuSellUnit Parameter of type {@code SkuSellUnit} used by this operation.
     * @return {@code List<SkuUnitBarcode>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<SkuUnitBarcode> findBySkuSellUnitOrderByIsPrimaryDescIdAsc(SkuSellUnit skuSellUnit);
    /**
     * Executes the findByBarcode operation.
     *
     * @param barcode Parameter of type {@code String} used by this operation.
     * @return {@code Optional<SkuUnitBarcode>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<SkuUnitBarcode> findByBarcode(String barcode);
    /**
     * Executes the findByBarcodeIgnoreCase operation.
     *
     * @param barcode Parameter of type {@code String} used by this operation.
     * @return {@code Optional<SkuUnitBarcode>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<SkuUnitBarcode> findByBarcodeIgnoreCase(String barcode);
}
