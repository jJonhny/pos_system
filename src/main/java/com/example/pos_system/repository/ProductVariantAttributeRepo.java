package com.example.pos_system.repository;

import com.example.pos_system.entity.ProductVariant;
import com.example.pos_system.entity.ProductVariantAttribute;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ProductVariantAttributeRepo extends JpaRepository<ProductVariantAttribute, Long> {
    /**
     * Executes the findByVariantOrderByGroup_SortOrderAsc operation.
     *
     * @param variant Parameter of type {@code ProductVariant} used by this operation.
     * @return {@code List<ProductVariantAttribute>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<ProductVariantAttribute> findByVariantOrderByGroup_SortOrderAsc(ProductVariant variant);
    /**
     * Executes the deleteByVariant operation.
     *
     * @param variant Parameter of type {@code ProductVariant} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    void deleteByVariant(ProductVariant variant);
}
