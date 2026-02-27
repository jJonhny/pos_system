package com.example.pos_system.repository;

import com.example.pos_system.entity.AuditEvent;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface AuditEventRepo extends JpaRepository<AuditEvent, Long>, JpaSpecificationExecutor<AuditEvent> {
    /**
     * Executes the findTopByActionTypeOrderByTimestampDesc operation.
     *
     * @param actionType Parameter of type {@code String} used by this operation.
     * @return {@code Optional<AuditEvent>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<AuditEvent> findTopByActionTypeOrderByTimestampDesc(String actionType);

    /**
     * Executes the findDistinctActionTypes operation.
     *
     * @return {@code List<String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findDistinctActionTypes operation.
     *
     * @return {@code List<String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findDistinctActionTypes operation.
     *
     * @return {@code List<String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Query("select distinct a.actionType from AuditEvent a order by a.actionType asc")
    List<String> findDistinctActionTypes();

    /**
     * Executes the findDistinctTargetTypes operation.
     *
     * @return {@code List<String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findDistinctTargetTypes operation.
     *
     * @return {@code List<String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findDistinctTargetTypes operation.
     *
     * @return {@code List<String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Query("select distinct a.targetType from AuditEvent a order by a.targetType asc")
    List<String> findDistinctTargetTypes();

    /**
     * Executes the countByActionType operation.
     *
     * @param actionType Parameter of type {@code String} used by this operation.
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    long countByActionType(String actionType);
}
