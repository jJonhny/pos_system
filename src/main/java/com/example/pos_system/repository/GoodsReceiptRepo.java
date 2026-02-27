package com.example.pos_system.repository;

import com.example.pos_system.entity.GoodsReceipt;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface GoodsReceiptRepo extends JpaRepository<GoodsReceipt, Long> {
    /**
     * Executes the findAllByOrderByReceivedAtDesc operation.
     *
     * @return {@code List<GoodsReceipt>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findAllByOrderByReceivedAtDesc operation.
     *
     * @return {@code List<GoodsReceipt>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findAllByOrderByReceivedAtDesc operation.
     *
     * @return {@code List<GoodsReceipt>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @EntityGraph(attributePaths = {"purchaseOrder", "purchaseOrder.supplier", "items", "items.product"})
    List<GoodsReceipt> findAllByOrderByReceivedAtDesc();

    /**
     * Executes the findDetailedById operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<GoodsReceipt>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findDetailedById operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<GoodsReceipt>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findDetailedById operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<GoodsReceipt>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @EntityGraph(attributePaths = {"purchaseOrder", "purchaseOrder.supplier", "items", "items.product"})
    Optional<GoodsReceipt> findDetailedById(Long id);
}
