package com.devcore.pos_system.modules.currency.application;

import com.devcore.pos_system.modules.currency.domain.Currency;
import com.devcore.pos_system.modules.currency.domain.CurrencyRateLog;
import com.devcore.pos_system.modules.currency.infrastructure.CurrencyRateLogRepo;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Service
@Transactional(readOnly = true)
public class CurrencyAnalyticsService {
    private static final int RATE_BAR_LIMIT = 10;
    private static final int TOP_VOLATILITY_LIMIT = 8;
    private static final int TOP_FRESHNESS_LIMIT = 8;
    private static final int TREND_SERIES_LIMIT = 4;
    private static final int TREND_POINT_COUNT = 12;

    private final CurrencyRateLogRepo rateLogRepo;

    /**
     * Executes the CurrencyAnalyticsService operation.
     * <p>Return value: A fully initialized CurrencyAnalyticsService instance.</p>
     *
     * @param rateLogRepo Parameter of type {@code CurrencyRateLogRepo} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public CurrencyAnalyticsService(CurrencyRateLogRepo rateLogRepo) {
        this.rateLogRepo = rateLogRepo;
    }

    /**
     * Executes the build operation.
     *
     * @param currencies Parameter of type {@code List<Currency>} used by this operation.
     * @param baseCurrency Parameter of type {@code Currency} used by this operation.
     * @return {@code CurrencyAnalyticsStats} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public CurrencyAnalyticsStats build(List<Currency> currencies, Currency baseCurrency) {
        List<Currency> safeCurrencies = currencies == null ? List.of() : currencies.stream()
                .filter(Objects::nonNull)
                .toList();
        LocalDateTime now = LocalDateTime.now();

        List<Currency> activeCurrencies = safeCurrencies.stream()
                .filter(c -> Boolean.TRUE.equals(c.getActive()) || Boolean.TRUE.equals(c.getBase()))
                .sorted(Comparator.comparing(Currency::getCode, String.CASE_INSENSITIVE_ORDER))
                .toList();

        List<Currency> nonBaseActive = activeCurrencies.stream()
                .filter(c -> !Boolean.TRUE.equals(c.getBase()))
                .toList();

        Currency strongest = nonBaseActive.stream()
                .filter(c -> safeRate(c).compareTo(BigDecimal.ZERO) > 0)
                .max(Comparator.comparing(this::safeRate))
                .orElse(null);

        Currency weakest = nonBaseActive.stream()
                .filter(c -> safeRate(c).compareTo(BigDecimal.ZERO) > 0)
                .min(Comparator.comparing(this::safeRate))
                .orElse(null);

        double rateSpreadPercent = 0.0;
        if (strongest != null && weakest != null && safeRate(weakest).compareTo(BigDecimal.ZERO) > 0) {
            BigDecimal spread = safeRate(strongest)
                    .divide(safeRate(weakest), 8, RoundingMode.HALF_UP)
                    .subtract(BigDecimal.ONE)
                    .multiply(new BigDecimal("100"));
            rateSpreadPercent = round2(spread.doubleValue());
        }

        Map<String, Double> volatilityByCode = new LinkedHashMap<>();
        for (Currency currency : nonBaseActive) {
            List<CurrencyRateLog> logs = rateLogRepo.findTop30ByCurrencyCodeOrderByCreatedAtDesc(currency.getCode());
            Collections.reverse(logs);
            volatilityByCode.put(currency.getCode(), round2(averageAbsolutePctChange(logs)));
        }

        String mostVolatileCode = "-";
        double mostVolatilePercent = 0.0;
        if (!volatilityByCode.isEmpty()) {
            Map.Entry<String, Double> top = volatilityByCode.entrySet().stream()
                    .max(Map.Entry.comparingByValue())
                    .orElse(null);
            if (top != null) {
                mostVolatileCode = top.getKey();
                mostVolatilePercent = round2(top.getValue());
            }
        }

        double averageVolatilityPercent = round2(volatilityByCode.values().stream()
                .mapToDouble(Double::doubleValue)
                .average()
                .orElse(0.0));

        List<Map.Entry<String, Double>> staleEntries = nonBaseActive.stream()
                .map(currency -> Map.entry(currency.getCode(), round2(hoursSince(currency.getUpdatedAt(), now))))
                .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                .toList();
        String stalestCode = staleEntries.isEmpty() ? "-" : staleEntries.get(0).getKey();
        double stalestHours = staleEntries.isEmpty() ? 0.0 : staleEntries.get(0).getValue();

        List<Currency> rateBarCurrencies = nonBaseActive.stream()
                .sorted(Comparator.comparing((Currency c) -> safeRate(c)).reversed())
                .limit(RATE_BAR_LIMIT)
                .toList();
        List<String> rateBarLabels = rateBarCurrencies.stream().map(Currency::getCode).toList();
        List<Double> rateBarValues = rateBarCurrencies.stream().map(c -> round6(safeRate(c).doubleValue())).toList();

        List<Map.Entry<String, Double>> volatilityEntries = volatilityByCode.entrySet().stream()
                .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                .limit(TOP_VOLATILITY_LIMIT)
                .toList();
        List<String> volatilityLabels = volatilityEntries.stream().map(Map.Entry::getKey).toList();
        List<Double> volatilityValues = volatilityEntries.stream().map(Map.Entry::getValue).toList();

        List<String> freshnessLabels = staleEntries.stream()
                .limit(TOP_FRESHNESS_LIMIT)
                .map(Map.Entry::getKey)
                .toList();
        List<Double> freshnessHours = staleEntries.stream()
                .limit(TOP_FRESHNESS_LIMIT)
                .map(Map.Entry::getValue)
                .toList();

        List<String> trendCodes = selectTrendCodes(nonBaseActive, volatilityByCode);
        List<String> trendLabels = buildTrendLabels(TREND_POINT_COUNT);
        List<List<Double>> trendSeries = new ArrayList<>();
        for (String code : trendCodes) {
            List<CurrencyRateLog> logs = rateLogRepo.findTop30ByCurrencyCodeOrderByCreatedAtDesc(code);
            Collections.reverse(logs);
            trendSeries.add(buildNormalizedTrend(logs, TREND_POINT_COUNT));
        }

        Map<String, Double> converterRates = new LinkedHashMap<>();
        for (Currency currency : activeCurrencies) {
            converterRates.put(currency.getCode(), round8(safeRate(currency).doubleValue()));
        }
        if (baseCurrency != null && baseCurrency.getCode() != null) {
            converterRates.putIfAbsent(baseCurrency.getCode(), 1.0);
        }
        List<String> converterCodes = new ArrayList<>(converterRates.keySet());

        return new CurrencyAnalyticsStats(
                safeCurrencies.size(),
                activeCurrencies.size(),
                Math.max(0, safeCurrencies.size() - activeCurrencies.size()),
                strongest == null ? "-" : strongest.getCode(),
                weakest == null ? "-" : weakest.getCode(),
                rateSpreadPercent,
                mostVolatileCode,
                mostVolatilePercent,
                averageVolatilityPercent,
                stalestCode,
                stalestHours,
                rateBarLabels,
                rateBarValues,
                volatilityLabels,
                volatilityValues,
                freshnessLabels,
                freshnessHours,
                trendLabels,
                trendCodes,
                trendSeries,
                converterCodes,
                converterRates
        );
    }

    /**
     * Executes the selectTrendCodes operation.
     *
     * @param nonBaseActive Parameter of type {@code List<Currency>} used by this operation.
     * @param volatilityByCode Parameter of type {@code Map<String, Double>} used by this operation.
     * @return {@code List<String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<String> selectTrendCodes(List<Currency> nonBaseActive, Map<String, Double> volatilityByCode) {
        List<String> byVolatility = volatilityByCode.entrySet().stream()
                .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                .map(Map.Entry::getKey)
                .limit(TREND_SERIES_LIMIT)
                .toList();
        if (!byVolatility.isEmpty()) {
            return byVolatility;
        }
        return nonBaseActive.stream()
                .map(Currency::getCode)
                .limit(TREND_SERIES_LIMIT)
                .toList();
    }

    /**
     * Executes the buildTrendLabels operation.
     *
     * @param count Parameter of type {@code int} used by this operation.
     * @return {@code List<String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<String> buildTrendLabels(int count) {
        List<String> labels = new ArrayList<>();
        for (int i = count - 1; i >= 0; i--) {
            labels.add("T-" + i);
        }
        return labels;
    }

    /**
     * Executes the buildNormalizedTrend operation.
     *
     * @param logs Parameter of type {@code List<CurrencyRateLog>} used by this operation.
     * @param targetPoints Parameter of type {@code int} used by this operation.
     * @return {@code List<Double>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<Double> buildNormalizedTrend(List<CurrencyRateLog> logs, int targetPoints) {
        List<BigDecimal> values = logs == null ? List.of() : logs.stream()
                .map(log -> log == null ? BigDecimal.ZERO : safeRate(log.getRateToBase()))
                .toList();
        if (values.isEmpty()) {
            return Collections.nCopies(targetPoints, null);
        }

        int start = Math.max(0, values.size() - targetPoints);
        List<BigDecimal> trimmed = values.subList(start, values.size());
        BigDecimal baseline = trimmed.stream()
                .filter(v -> v.compareTo(BigDecimal.ZERO) > 0)
                .findFirst()
                .orElse(BigDecimal.ONE);
        if (baseline.compareTo(BigDecimal.ZERO) <= 0) {
            baseline = BigDecimal.ONE;
        }

        List<Double> normalized = new ArrayList<>();
        int padCount = targetPoints - trimmed.size();
        for (int i = 0; i < padCount; i++) {
            normalized.add(null);
        }
        for (BigDecimal value : trimmed) {
            BigDecimal index = value.multiply(new BigDecimal("100")).divide(baseline, 6, RoundingMode.HALF_UP);
            normalized.add(round4(index.doubleValue()));
        }
        return normalized;
    }

    /**
     * Executes the averageAbsolutePctChange operation.
     *
     * @param logs Parameter of type {@code List<CurrencyRateLog>} used by this operation.
     * @return {@code double} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private double averageAbsolutePctChange(List<CurrencyRateLog> logs) {
        if (logs == null || logs.size() < 2) return 0.0;
        List<Double> changes = new ArrayList<>();
        for (int i = 1; i < logs.size(); i++) {
            BigDecimal prev = safeRate(logs.get(i - 1).getRateToBase());
            BigDecimal curr = safeRate(logs.get(i).getRateToBase());
            if (prev.compareTo(BigDecimal.ZERO) <= 0) continue;
            BigDecimal pct = curr.subtract(prev)
                    .divide(prev, 8, RoundingMode.HALF_UP)
                    .multiply(new BigDecimal("100"))
                    .abs();
            changes.add(pct.doubleValue());
        }
        if (changes.isEmpty()) return 0.0;
        return changes.stream().mapToDouble(Double::doubleValue).average().orElse(0.0);
    }

    /**
     * Executes the hoursSince operation.
     *
     * @param timestamp Parameter of type {@code LocalDateTime} used by this operation.
     * @param now Parameter of type {@code LocalDateTime} used by this operation.
     * @return {@code double} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private double hoursSince(LocalDateTime timestamp, LocalDateTime now) {
        if (timestamp == null || now == null || timestamp.isAfter(now)) return 0.0;
        long minutes = Duration.between(timestamp, now).toMinutes();
        return minutes / 60.0;
    }

    /**
     * Executes the safeRate operation.
     *
     * @param currency Parameter of type {@code Currency} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeRate(Currency currency) {
        if (currency == null) return BigDecimal.ZERO;
        return safeRate(currency.getRateToBase());
    }

    /**
     * Executes the safeRate operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeRate(BigDecimal value) {
        if (value == null || value.compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        return value;
    }

    /**
     * Executes the round2 operation.
     *
     * @param value Parameter of type {@code double} used by this operation.
     * @return {@code double} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private double round2(double value) {
        return BigDecimal.valueOf(value).setScale(2, RoundingMode.HALF_UP).doubleValue();
    }

    /**
     * Executes the round4 operation.
     *
     * @param value Parameter of type {@code double} used by this operation.
     * @return {@code double} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private double round4(double value) {
        return BigDecimal.valueOf(value).setScale(4, RoundingMode.HALF_UP).doubleValue();
    }

    /**
     * Executes the round6 operation.
     *
     * @param value Parameter of type {@code double} used by this operation.
     * @return {@code double} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private double round6(double value) {
        return BigDecimal.valueOf(value).setScale(6, RoundingMode.HALF_UP).doubleValue();
    }

    /**
     * Executes the round8 operation.
     *
     * @param value Parameter of type {@code double} used by this operation.
     * @return {@code double} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private double round8(double value) {
        return BigDecimal.valueOf(value).setScale(8, RoundingMode.HALF_UP).doubleValue();
    }
}
