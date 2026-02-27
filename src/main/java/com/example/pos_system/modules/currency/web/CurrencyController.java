package com.example.pos_system.modules.currency.web;

import com.example.pos_system.modules.currency.application.CurrencyAnalyticsService;
import com.example.pos_system.modules.currency.application.CurrencyAnalyticsStats;
import com.example.pos_system.modules.currency.application.CurrencyService;
import com.example.pos_system.modules.currency.domain.Currency;
import com.example.pos_system.modules.currency.domain.CurrencyRateLog;
import com.example.pos_system.modules.currency.infrastructure.CurrencyRateLogRepo;
import com.example.pos_system.modules.currency.infrastructure.CurrencyRepo;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.time.format.DateTimeFormatter;

@Controller
@RequestMapping("/currencies")
public class CurrencyController {
    private final CurrencyRepo currencyRepo;
    private final CurrencyService currencyService;
    private final CurrencyAnalyticsService currencyAnalyticsService;
    private final CurrencyRateLogRepo rateLogRepo;
    private static final DateTimeFormatter RATE_FMT = DateTimeFormatter.ofPattern("MM-dd HH:mm");

    /**
     * Executes the CurrencyController operation.
     * <p>Return value: A fully initialized CurrencyController instance.</p>
     *
     * @param currencyRepo Parameter of type {@code CurrencyRepo} used by this operation.
     * @param currencyService Parameter of type {@code CurrencyService} used by this operation.
     * @param currencyAnalyticsService Parameter of type {@code CurrencyAnalyticsService} used by this operation.
     * @param rateLogRepo Parameter of type {@code CurrencyRateLogRepo} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public CurrencyController(CurrencyRepo currencyRepo,
                              CurrencyService currencyService,
                              CurrencyAnalyticsService currencyAnalyticsService,
                              CurrencyRateLogRepo rateLogRepo) {
        this.currencyRepo = currencyRepo;
        this.currencyService = currencyService;
        this.currencyAnalyticsService = currencyAnalyticsService;
        this.rateLogRepo = rateLogRepo;
    }

    /**
     * Executes the list operation.
     *
     * @param chartCode Parameter of type {@code String} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the list operation.
     *
     * @param chartCode Parameter of type {@code String} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the list operation.
     *
     * @param chartCode Parameter of type {@code String} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping
    public String list(@RequestParam(required = false) String chartCode, Model model) {
        List<Currency> currencies = currencyService.getAllCurrencies();
        model.addAttribute("currencies", currencies);
        Currency base = currencyService.getBaseCurrency();
        model.addAttribute("baseCurrency", base);

        String selectedCode = chartCode;
        if (selectedCode == null || selectedCode.isBlank()) {
            selectedCode = currencies.stream()
                    .filter(c -> !Boolean.TRUE.equals(c.getBase()))
                    .map(Currency::getCode)
                    .findFirst()
                    .orElse(base != null ? base.getCode() : null);
        }
        model.addAttribute("chartCode", selectedCode);

        if (selectedCode != null) {
            List<CurrencyRateLog> logs =
                    rateLogRepo.findTop30ByCurrencyCodeOrderByCreatedAtDesc(selectedCode);
            Collections.reverse(logs);
            List<String> labels = logs.stream()
                    .map(log -> log.getCreatedAt() == null ? "" : log.getCreatedAt().format(RATE_FMT))
                    .collect(Collectors.toList());
            List<BigDecimal> values = logs.stream()
                    .map(log -> log.getRateToBase() == null ? BigDecimal.ZERO : log.getRateToBase())
                    .collect(Collectors.toList());
            model.addAttribute("rateLabels", labels);
            model.addAttribute("rateValues", values);
        } else {
            model.addAttribute("rateLabels", List.of());
            model.addAttribute("rateValues", List.of());
        }

        CurrencyAnalyticsStats analytics = currencyAnalyticsService.build(currencies, base);
        model.addAttribute("totalCurrencies", analytics.totalCurrencies());
        model.addAttribute("activeCurrencies", analytics.activeCurrencies());
        model.addAttribute("inactiveCurrencies", analytics.inactiveCurrencies());
        model.addAttribute("strongestCode", analytics.strongestCode());
        model.addAttribute("weakestCode", analytics.weakestCode());
        model.addAttribute("rateSpreadPercent", analytics.rateSpreadPercent());
        model.addAttribute("mostVolatileCode", analytics.mostVolatileCode());
        model.addAttribute("mostVolatilePercent", analytics.mostVolatilePercent());
        model.addAttribute("averageVolatilityPercent", analytics.averageVolatilityPercent());
        model.addAttribute("stalestCode", analytics.stalestCode());
        model.addAttribute("stalestHours", analytics.stalestHours());
        model.addAttribute("rateBarLabels", analytics.rateBarLabels());
        model.addAttribute("rateBarValues", analytics.rateBarValues());
        model.addAttribute("volatilityLabels", analytics.volatilityLabels());
        model.addAttribute("volatilityValues", analytics.volatilityValues());
        model.addAttribute("freshnessLabels", analytics.freshnessLabels());
        model.addAttribute("freshnessHours", analytics.freshnessHours());
        model.addAttribute("trendLabels", analytics.trendLabels());
        model.addAttribute("trendCodes", analytics.trendCodes());
        model.addAttribute("trendSeries", analytics.trendSeries());
        model.addAttribute("converterCodes", analytics.converterCodes());
        model.addAttribute("converterRates", analytics.converterRates());
        return "currencies/list";
    }

    /**
     * Executes the create operation.
     *
     * @param code Parameter of type {@code String} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param symbol Parameter of type {@code String} used by this operation.
     * @param rateToBase Parameter of type {@code BigDecimal} used by this operation.
     * @param fractionDigits Parameter of type {@code Integer} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the create operation.
     *
     * @param code Parameter of type {@code String} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param symbol Parameter of type {@code String} used by this operation.
     * @param rateToBase Parameter of type {@code BigDecimal} used by this operation.
     * @param fractionDigits Parameter of type {@code Integer} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the create operation.
     *
     * @param code Parameter of type {@code String} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param symbol Parameter of type {@code String} used by this operation.
     * @param rateToBase Parameter of type {@code BigDecimal} used by this operation.
     * @param fractionDigits Parameter of type {@code Integer} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping
    public String create(@RequestParam String code,
                         @RequestParam String name,
                         @RequestParam(required = false) String symbol,
                         @RequestParam(required = false) BigDecimal rateToBase,
                         @RequestParam(required = false) Integer fractionDigits,
                         @RequestParam(required = false) Boolean active,
                         RedirectAttributes redirectAttributes) {
        String normalized = code == null ? "" : code.trim().toUpperCase();
        if (normalized.isBlank()) {
            redirectAttributes.addFlashAttribute("error", "Currency code is required.");
            return "redirect:/currencies";
        }
        if (currencyRepo.existsByCodeIgnoreCase(normalized)) {
            redirectAttributes.addFlashAttribute("error", "Currency code already exists.");
            return "redirect:/currencies";
        }
        if (name == null || name.isBlank()) {
            redirectAttributes.addFlashAttribute("error", "Currency name is required.");
            return "redirect:/currencies";
        }
        if (rateToBase == null || rateToBase.compareTo(BigDecimal.ZERO) <= 0) {
            redirectAttributes.addFlashAttribute("error", "Rate must be greater than zero.");
            return "redirect:/currencies";
        }
        currencyService.createCurrency(normalized, name, symbol, rateToBase, fractionDigits, active);
        redirectAttributes.addFlashAttribute("success", "Currency added.");
        return "redirect:/currencies";
    }

    /**
     * Executes the update operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param symbol Parameter of type {@code String} used by this operation.
     * @param rateToBase Parameter of type {@code BigDecimal} used by this operation.
     * @param fractionDigits Parameter of type {@code Integer} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the update operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param symbol Parameter of type {@code String} used by this operation.
     * @param rateToBase Parameter of type {@code BigDecimal} used by this operation.
     * @param fractionDigits Parameter of type {@code Integer} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the update operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param symbol Parameter of type {@code String} used by this operation.
     * @param rateToBase Parameter of type {@code BigDecimal} used by this operation.
     * @param fractionDigits Parameter of type {@code Integer} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/{id}")
    public String update(@PathVariable Long id,
                         @RequestParam String name,
                         @RequestParam(required = false) String symbol,
                         @RequestParam(required = false) BigDecimal rateToBase,
                         @RequestParam(required = false) Integer fractionDigits,
                         @RequestParam(required = false) Boolean active,
                         RedirectAttributes redirectAttributes) {
        Currency updated = currencyService.updateCurrency(id, name, symbol, rateToBase, fractionDigits, active);
        if (updated == null) {
            redirectAttributes.addFlashAttribute("error", "Currency not found.");
        } else {
            redirectAttributes.addFlashAttribute("success", "Currency updated.");
        }
        return "redirect:/currencies";
    }

    /**
     * Executes the setBase operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the setBase operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the setBase operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/{id}/base")
    public String setBase(@PathVariable Long id, RedirectAttributes redirectAttributes) {
        if (currencyService.setBaseCurrency(id)) {
            redirectAttributes.addFlashAttribute("success", "Base currency updated.");
        } else {
            redirectAttributes.addFlashAttribute("error", "Currency not found.");
        }
        return "redirect:/currencies";
    }

    /**
     * Executes the refreshRates operation.
     *
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the refreshRates operation.
     *
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the refreshRates operation.
     *
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/refresh")
    public String refreshRates(RedirectAttributes redirectAttributes) {
        try {
            int updated = currencyService.refreshRates();
            if (updated == 0) {
                redirectAttributes.addFlashAttribute("error", "No rates updated. Configure app.currency.rate-url first.");
            } else {
                redirectAttributes.addFlashAttribute("success", "Updated " + updated + " currency rates.");
            }
        } catch (Exception ex) {
            redirectAttributes.addFlashAttribute("error", ex.getMessage());
        }
        return "redirect:/currencies";
    }
}
