package com.example.pos_system.repository;

import com.example.pos_system.entity.PurchaseOrder;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface PurchaseOrderRepo extends JpaRepository<PurchaseOrder, Long> {
    /**
     * Executes the findAllByOrderByCreatedAtDesc operation.
     *
     * @return {@code List<PurchaseOrder>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findAllByOrderByCreatedAtDesc operation.
     *
     * @return {@code List<PurchaseOrder>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findAllByOrderByCreatedAtDesc operation.
     *
     * @return {@code List<PurchaseOrder>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @EntityGraph(attributePaths = {"supplier"})
    List<PurchaseOrder> findAllByOrderByCreatedAtDesc();

    /**
     * Executes the findDetailedById operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<PurchaseOrder>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findDetailedById operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<PurchaseOrder>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findDetailedById operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<PurchaseOrder>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @EntityGraph(attributePaths = {"supplier", "items", "items.product"})
    Optional<PurchaseOrder> findDetailedById(Long id);
}
