package com.example.pos_system.repository;

import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.ProductVariantExclusion;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface ProductVariantExclusionRepo extends JpaRepository<ProductVariantExclusion, Long> {
    /**
     * Executes the findByProductAndActiveTrue operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code List<ProductVariantExclusion>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<ProductVariantExclusion> findByProductAndActiveTrue(Product product);
    /**
     * Executes the findByProductAndCombinationHash operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @param combinationHash Parameter of type {@code String} used by this operation.
     * @return {@code Optional<ProductVariantExclusion>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<ProductVariantExclusion> findByProductAndCombinationHash(Product product, String combinationHash);
}
