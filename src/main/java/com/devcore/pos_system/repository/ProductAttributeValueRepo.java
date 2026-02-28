package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.ProductAttributeValue;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ProductAttributeValueRepo extends JpaRepository<ProductAttributeValue, Long> {
    /**
     * Executes the findByProductAndActiveTrueOrderByGroup_SortOrderAscValue_SortOrderAsc operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code List<ProductAttributeValue>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<ProductAttributeValue> findByProductAndActiveTrueOrderByGroup_SortOrderAscValue_SortOrderAsc(Product product);
    /**
     * Executes the findByProductOrderByGroup_SortOrderAscValue_SortOrderAsc operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code List<ProductAttributeValue>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<ProductAttributeValue> findByProductOrderByGroup_SortOrderAscValue_SortOrderAsc(Product product);
    /**
     * Executes the deleteByProduct operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    void deleteByProduct(Product product);
}
