package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.ProductVariant;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

public interface ProductVariantRepo extends JpaRepository<ProductVariant, Long> {
    /**
     * Executes the findByProductAndArchivedFalseOrderByIdAsc operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code List<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<ProductVariant> findByProductAndArchivedFalseOrderByIdAsc(Product product);
    /**
     * Executes the findByProductAndArchivedFalseAndEnabledTrueAndImpossibleFalseOrderByIdAsc operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code List<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<ProductVariant> findByProductAndArchivedFalseAndEnabledTrueAndImpossibleFalseOrderByIdAsc(Product product);
    /**
     * Executes the findByProductOrderByIdAsc operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code List<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<ProductVariant> findByProductOrderByIdAsc(Product product);
    /**
     * Executes the findByProductAndCombinationHash operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @param combinationHash Parameter of type {@code String} used by this operation.
     * @return {@code Optional<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<ProductVariant> findByProductAndCombinationHash(Product product, String combinationHash);
    /**
     * Executes the findByIdIn operation.
     *
     * @param ids Parameter of type {@code Collection<Long>} used by this operation.
     * @return {@code List<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<ProductVariant> findByIdIn(Collection<Long> ids);
    /**
     * Executes the findByBarcode operation.
     *
     * @param barcode Parameter of type {@code String} used by this operation.
     * @return {@code Optional<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<ProductVariant> findByBarcode(String barcode);
    /**
     * Executes the findBySkuIgnoreCase operation.
     *
     * @param sku Parameter of type {@code String} used by this operation.
     * @return {@code Optional<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<ProductVariant> findBySkuIgnoreCase(String sku);

    /**
     * Executes the findByIdForUpdate operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findByIdForUpdate operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findByIdForUpdate operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<ProductVariant>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("select v from ProductVariant v join fetch v.product where v.id = :id")
    Optional<ProductVariant> findByIdForUpdate(@Param("id") Long id);
}
