package com.example.pos_system.modules.currency.infrastructure;

import com.example.pos_system.modules.currency.domain.Currency;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface CurrencyRepo extends JpaRepository<Currency, Long> {
    /**
     * Executes the findByCodeIgnoreCase operation.
     *
     * @param code Parameter of type {@code String} used by this operation.
     * @return {@code Optional<Currency>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<Currency> findByCodeIgnoreCase(String code);
    /**
     * Executes the existsByCodeIgnoreCase operation.
     *
     * @param code Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    boolean existsByCodeIgnoreCase(String code);
    /**
     * Executes the findByBaseTrue operation.
     *
     * @return {@code Optional<Currency>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<Currency> findByBaseTrue();
    /**
     * Executes the findByActiveTrueOrderByCodeAsc operation.
     *
     * @return {@code List<Currency>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<Currency> findByActiveTrueOrderByCodeAsc();
    /**
     * Executes the findAllByOrderByCodeAsc operation.
     *
     * @return {@code List<Currency>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<Currency> findAllByOrderByCodeAsc();
}
