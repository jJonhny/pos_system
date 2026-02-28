package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.Supplier;
import com.devcore.pos_system.entity.SupplierStatus;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface SupplierRepo extends JpaRepository<Supplier, Long> {
    /**
     * Executes the findByStatus operation.
     *
     * @param status Parameter of type {@code SupplierStatus} used by this operation.
     * @param sort Parameter of type {@code Sort} used by this operation.
     * @return {@code List<Supplier>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<Supplier> findByStatus(SupplierStatus status, Sort sort);
    /**
     * Executes the findByNameContainingIgnoreCase operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param sort Parameter of type {@code Sort} used by this operation.
     * @return {@code List<Supplier>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<Supplier> findByNameContainingIgnoreCase(String q, Sort sort);
    /**
     * Executes the findByNameContainingIgnoreCaseAndStatus operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code SupplierStatus} used by this operation.
     * @param sort Parameter of type {@code Sort} used by this operation.
     * @return {@code List<Supplier>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<Supplier> findByNameContainingIgnoreCaseAndStatus(String q, SupplierStatus status, Sort sort);
}
