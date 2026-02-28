package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.Product;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import jakarta.persistence.LockModeType;
import java.util.Optional;

public interface ProductRepo extends JpaRepository<Product, Long>, JpaSpecificationExecutor<Product> {
    /**
     * Executes the findAll operation.
     *
     * @param spec Parameter of type {@code org.springframework.data.jpa.domain.Specification<Product>} used by this operation.
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findAll operation.
     *
     * @param spec Parameter of type {@code org.springframework.data.jpa.domain.Specification<Product>} used by this operation.
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findAll operation.
     *
     * @param spec Parameter of type {@code org.springframework.data.jpa.domain.Specification<Product>} used by this operation.
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Override
    @EntityGraph(attributePaths = "category")
    Page<Product> findAll(org.springframework.data.jpa.domain.Specification<Product> spec, Pageable pageable);

    /**
     * Executes the findAll operation.
     *
     * @param spec Parameter of type {@code org.springframework.data.jpa.domain.Specification<Product>} used by this operation.
     * @param sort Parameter of type {@code Sort} used by this operation.
     * @return {@code java.util.List<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findAll operation.
     *
     * @param spec Parameter of type {@code org.springframework.data.jpa.domain.Specification<Product>} used by this operation.
     * @param sort Parameter of type {@code Sort} used by this operation.
     * @return {@code java.util.List<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findAll operation.
     *
     * @param spec Parameter of type {@code org.springframework.data.jpa.domain.Specification<Product>} used by this operation.
     * @param sort Parameter of type {@code Sort} used by this operation.
     * @return {@code java.util.List<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @EntityGraph(attributePaths = "category")
    java.util.List<Product> findAll(org.springframework.data.jpa.domain.Specification<Product> spec, Sort sort);

    /**
     * Executes the findByActiveTrue operation.
     *
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Page<Product> findByActiveTrue(Pageable pageable);
    /**
     * Executes the findByActiveTrueAndNameContainingIgnoreCase operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Page<Product> findByActiveTrueAndNameContainingIgnoreCase(String q, Pageable pageable);
    /**
     * Executes the findByActiveTrueAndCategory_Id operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Page<Product> findByActiveTrueAndCategory_Id(Long categoryId, Pageable pageable);
    /**
     * Executes the findByCategory_Id operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Page<Product> findByCategory_Id(Long categoryId, Pageable pageable);
    /**
     * Executes the findByBarcode operation.
     *
     * @param barcode Parameter of type {@code String} used by this operation.
     * @return {@code Optional<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<Product> findByBarcode(String barcode);
    /**
     * Executes the findBySkuIgnoreCase operation.
     *
     * @param sku Parameter of type {@code String} used by this operation.
     * @return {@code Optional<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<Product> findBySkuIgnoreCase(String sku);
    /**
     * Executes the findByIdForUpdate operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findByIdForUpdate operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findByIdForUpdate operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("select p from Product p where p.id = :id")
    Optional<Product> findByIdForUpdate(@Param("id") Long id);
    /**
     * Executes the existsByCategory_Id operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    boolean existsByCategory_Id(Long categoryId);
    /**
     * Executes the findByCategory_Id operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param sort Parameter of type {@code Sort} used by this operation.
     * @return {@code java.util.List<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    java.util.List<Product> findByCategory_Id(Long categoryId, Sort sort);

    /**
     * Executes the countByCategory operation.
     *
     * @return {@code java.util.List<CategoryCount>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the countByCategory operation.
     *
     * @return {@code java.util.List<CategoryCount>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the countByCategory operation.
     *
     * @return {@code java.util.List<CategoryCount>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Query("select p.category.id as categoryId, count(p) as count from Product p where p.category is not null group by p.category.id")
    java.util.List<CategoryCount> countByCategory();

            /**
             * Executes the count operation.
             *
             * @param p Parameter of type {@code Object} used by this operation.
             * @return {@code select p.category.id as categoryId,} Result produced by this operation.
             * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
             * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
             */
            /**
             * Executes the count operation.
             *
             * @param p Parameter of type {@code Object} used by this operation.
             * @return {@code select p.category.id as categoryId,} Result produced by this operation.
             * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
             * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
             */
            /**
             * Executes the count operation.
             *
             * @param p Parameter of type {@code Object} used by this operation.
             * @return {@code select p.category.id as categoryId,} Result produced by this operation.
             * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
             * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
             */
    @Query("""
            select p.category.id as categoryId, count(p) as count
            from Product p
            where p.category is not null
              and p.stockQty is not null
              and p.lowStockThreshold is not null
              and p.stockQty <= p.lowStockThreshold
            group by p.category.id
            """)
    /**
     * Executes the countLowStockByCategory operation.
     *
     * @return {@code java.util.List<CategoryCount>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    java.util.List<CategoryCount> countLowStockByCategory();

    /**
     * Executes the countLowStock operation.
     *
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the countLowStock operation.
     *
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the countLowStock operation.
     *
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Query("select count(p) from Product p where p.stockQty is not null and p.lowStockThreshold is not null and p.stockQty <= p.lowStockThreshold")
    long countLowStock();

    /**
     * Executes the findLowStock operation.
     *
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findLowStock operation.
     *
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findLowStock operation.
     *
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Query("select p from Product p where p.stockQty is not null and p.lowStockThreshold is not null and p.stockQty <= p.lowStockThreshold")
    Page<Product> findLowStock(Pageable pageable);

    /**
     * Executes the findLowStockByCategoryId operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findLowStockByCategoryId operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findLowStockByCategoryId operation.
     *
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Query("select p from Product p where p.stockQty is not null and p.lowStockThreshold is not null and p.stockQty <= p.lowStockThreshold and p.category.id = :categoryId")
    Page<Product> findLowStockByCategoryId(@Param("categoryId") Long categoryId, Pageable pageable);

    @EntityGraph(attributePaths = "category")
    @Query("""
            select p
            from Product p
            left join p.category c
            where p.active = true
              and (:categoryId is null or c.id = :categoryId)
              and (:q is null
                    /**
                     * Executes the lower operation.
                     *
                     * @param name Parameter of type {@code p.} used by this operation.
                     * @return {@code or} Result produced by this operation.
                     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
                     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
                     */
                    or lower(p.name) like lower(concat(:q, '%'))
                    /**
                     * Executes the lower operation.
                     *
                     * @return {@code or} Result produced by this operation.
                     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
                     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
                     */
                    or lower(coalesce(p.sku, '')) like lower(concat(:q, '%'))
                    /**
                     * Executes the lower operation.
                     *
                     * @return {@code or} Result produced by this operation.
                     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
                     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
                     */
                    or lower(coalesce(p.barcode, '')) like lower(concat(:q, '%')))
              and (:cursorId is null or p.id < :cursorId)
            order by p.id desc
            """)
    /**
     * Executes the findActiveFeedBatch operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param categoryId Parameter of type {@code Long} used by this operation.
     * @param cursorId Parameter of type {@code Long} used by this operation.
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code java.util.List<Product>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    java.util.List<Product> findActiveFeedBatch(@Param("q") String q,
                                                @Param("categoryId") Long categoryId,
                                                @Param("cursorId") Long cursorId,
                                                Pageable pageable);

    interface CategoryCount {
        /**
         * Executes the getCategoryId operation.
         *
         * @return {@code Long} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        Long getCategoryId();
        /**
         * Executes the getCount operation.
         *
         * @return {@code Long} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        Long getCount();
    }
}
