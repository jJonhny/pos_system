package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.ShiftCashEvent;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ShiftCashEventRepo extends JpaRepository<ShiftCashEvent, Long> {
    /**
     * Executes the findByShift_IdOrderByCreatedAtAsc operation.
     *
     * @param shiftId Parameter of type {@code Long} used by this operation.
     * @return {@code List<ShiftCashEvent>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<ShiftCashEvent> findByShift_IdOrderByCreatedAtAsc(Long shiftId);
}
