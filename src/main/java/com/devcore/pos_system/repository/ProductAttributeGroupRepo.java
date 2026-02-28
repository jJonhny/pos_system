package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.ProductAttributeGroup;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ProductAttributeGroupRepo extends JpaRepository<ProductAttributeGroup, Long> {
    /**
     * Executes the findByProductOrderBySortOrderAscIdAsc operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code List<ProductAttributeGroup>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<ProductAttributeGroup> findByProductOrderBySortOrderAscIdAsc(Product product);
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
