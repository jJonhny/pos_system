package com.example.pos_system.repository;

import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SaleStatus;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface SaleRepo extends JpaRepository<Sale, Long> {
    /**
     * Executes the findByShift_Id operation.
     *
     * @param shiftId Parameter of type {@code Long} used by this operation.
     * @return {@code List<Sale>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<Sale> findByShift_Id(Long shiftId);
    /**
     * Executes the findByCustomer_Id operation.
     *
     * @param customerId Parameter of type {@code Long} used by this operation.
     * @return {@code List<Sale>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<Sale> findByCustomer_Id(Long customerId);
    /**
     * Executes the existsByCustomer_IdAndStatusNot operation.
     *
     * @param customerId Parameter of type {@code Long} used by this operation.
     * @param status Parameter of type {@code SaleStatus} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    boolean existsByCustomer_IdAndStatusNot(Long customerId, SaleStatus status);

    /**
     * Executes the findByIdForReceipt operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<Sale>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findByIdForReceipt operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<Sale>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findByIdForReceipt operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Optional<Sale>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @EntityGraph(attributePaths = {"items", "items.product", "customer", "shift"})
    @Query("select s from Sale s where s.id = :id")
    Optional<Sale> findByIdForReceipt(@Param("id") Long id);
}
