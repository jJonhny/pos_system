package com.example.pos_system.config;

import com.example.pos_system.modules.currency.application.CurrencyService;
import com.example.pos_system.modules.currency.infrastructure.CurrencyRepo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.List;

@Component
public class CurrencySeeder implements CommandLineRunner {
    private static final Logger log = LoggerFactory.getLogger(CurrencySeeder.class);

    private record Seed(String code, String name, String symbol, int decimals) {}

    private final CurrencyRepo currencyRepo;
    private final CurrencyService currencyService;

    /**
     * Executes the CurrencySeeder operation.
     * <p>Return value: A fully initialized CurrencySeeder instance.</p>
     *
     * @param currencyRepo Parameter of type {@code CurrencyRepo} used by this operation.
     * @param currencyService Parameter of type {@code CurrencyService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public CurrencySeeder(CurrencyRepo currencyRepo, CurrencyService currencyService) {
        this.currencyRepo = currencyRepo;
        this.currencyService = currencyService;
    }

    /**
     * Executes the run operation.
     *
     * @param args Parameter of type {@code String...} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the run operation.
     *
     * @param args Parameter of type {@code String...} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the run operation.
     *
     * @param args Parameter of type {@code String...} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Override
    public void run(String... args) {
        currencyService.ensureBaseCurrency();

        List<Seed> seeds = List.of(
                new Seed("USD", "US Dollar", "$", 2),
                new Seed("EUR", "Euro", "€", 2),
                new Seed("GBP", "British Pound", "£", 2),
                new Seed("JPY", "Japanese Yen", "¥", 0),
                new Seed("CNY", "Chinese Yuan", "¥", 2),
                new Seed("AUD", "Australian Dollar", "A$", 2),
                new Seed("CAD", "Canadian Dollar", "C$", 2),
                new Seed("CHF", "Swiss Franc", "CHF", 2),
                new Seed("SGD", "Singapore Dollar", "S$", 2),
                new Seed("KHR", "Cambodian Riel", "៛", 0)
        );

        int added = 0;
        for (Seed seed : seeds) {
            if (currencyRepo.existsByCodeIgnoreCase(seed.code())) {
                continue;
            }
            currencyService.createCurrency(
                    seed.code(),
                    seed.name(),
                    seed.symbol(),
                    BigDecimal.ONE,
                    seed.decimals(),
                    false
            );
            added++;
        }

        if (added > 0) {
            log.info("Seeded {} currencies (inactive). Update rates and activate as needed.", added);
        }

        try {
            int updated = currencyService.refreshRates();
            if (updated > 0) {
                log.info("Initial currency rate refresh updated {} currencies.", updated);
            }
        } catch (Exception ex) {
            log.warn("Initial currency rate refresh failed: {}", ex.getMessage());
        }
    }
}
