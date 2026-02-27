package com.example.pos_system.repository;

import com.example.pos_system.entity.TerminalSettings;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface TerminalSettingsRepo extends JpaRepository<TerminalSettings, Long> {
    /**
     * Executes the findByTerminalIdIgnoreCase operation.
     *
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code Optional<TerminalSettings>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<TerminalSettings> findByTerminalIdIgnoreCase(String terminalId);

    /**
     * Executes the findAllByOrderByNameAscTerminalIdAsc operation.
     *
     * @return {@code List<TerminalSettings>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<TerminalSettings> findAllByOrderByNameAscTerminalIdAsc();
}
