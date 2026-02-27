package com.example.pos_system.repository;

import com.example.pos_system.entity.StockMovement;
import com.example.pos_system.entity.StockMovementType;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.time.LocalDateTime;
import java.util.List;

public interface StockMovementRepo extends JpaRepository<StockMovement, Long>, JpaSpecificationExecutor<StockMovement> {
    /**
     * Executes the findByCreatedAtBetweenOrderByCreatedAtDesc operation.
     *
     * @param from Parameter of type {@code LocalDateTime} used by this operation.
     * @param to Parameter of type {@code LocalDateTime} used by this operation.
     * @return {@code List<StockMovement>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findByCreatedAtBetweenOrderByCreatedAtDesc operation.
     *
     * @param from Parameter of type {@code LocalDateTime} used by this operation.
     * @param to Parameter of type {@code LocalDateTime} used by this operation.
     * @return {@code List<StockMovement>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findByCreatedAtBetweenOrderByCreatedAtDesc operation.
     *
     * @param from Parameter of type {@code LocalDateTime} used by this operation.
     * @param to Parameter of type {@code LocalDateTime} used by this operation.
     * @return {@code List<StockMovement>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @EntityGraph(attributePaths = {"product", "product.category"})
    List<StockMovement> findByCreatedAtBetweenOrderByCreatedAtDesc(LocalDateTime from, LocalDateTime to);

    /**
     * Executes the countByType operation.
     *
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    long countByType(StockMovementType type);
}
