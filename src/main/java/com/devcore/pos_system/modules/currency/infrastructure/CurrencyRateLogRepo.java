package com.devcore.pos_system.modules.currency.infrastructure;

import com.devcore.pos_system.modules.currency.domain.CurrencyRateLog;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface CurrencyRateLogRepo extends JpaRepository<CurrencyRateLog, Long> {
    /**
     * Executes the findTop30ByCurrencyCodeOrderByCreatedAtDesc operation.
     *
     * @param currencyCode Parameter of type {@code String} used by this operation.
     * @return {@code List<CurrencyRateLog>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<CurrencyRateLog> findTop30ByCurrencyCodeOrderByCreatedAtDesc(String currencyCode);
}
