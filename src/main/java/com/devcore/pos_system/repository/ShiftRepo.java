package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.Shift;
import com.devcore.pos_system.entity.ShiftStatus;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

public interface ShiftRepo extends JpaRepository<Shift, Long> {
    /**
     * Executes the findByCashierUsernameAndStatus operation.
     *
     * @param cashierUsername Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code ShiftStatus} used by this operation.
     * @return {@code Optional<Shift>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<Shift> findByCashierUsernameAndStatus(String cashierUsername, ShiftStatus status);
    /**
     * Executes the findByCashierUsernameAndTerminalIdAndStatus operation.
     *
     * @param cashierUsername Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code ShiftStatus} used by this operation.
     * @return {@code Optional<Shift>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<Shift> findByCashierUsernameAndTerminalIdAndStatus(String cashierUsername, String terminalId, ShiftStatus status);
    /**
     * Executes the findByCashierUsernameOrderByOpenedAtDesc operation.
     *
     * @param cashierUsername Parameter of type {@code String} used by this operation.
     * @return {@code List<Shift>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<Shift> findByCashierUsernameOrderByOpenedAtDesc(String cashierUsername);
    /**
     * Executes the findByStatusOrderByOpenedAtDesc operation.
     *
     * @param status Parameter of type {@code ShiftStatus} used by this operation.
     * @return {@code List<Shift>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<Shift> findByStatusOrderByOpenedAtDesc(ShiftStatus status);
    /**
     * Executes the findByOpenedAtBetweenOrderByOpenedAtDesc operation.
     *
     * @param from Parameter of type {@code LocalDateTime} used by this operation.
     * @param to Parameter of type {@code LocalDateTime} used by this operation.
     * @return {@code List<Shift>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<Shift> findByOpenedAtBetweenOrderByOpenedAtDesc(LocalDateTime from, LocalDateTime to);
}
