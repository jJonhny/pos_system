package com.example.pos_system.dto;

import java.util.List;
import java.util.Map;

public record CurrencyAnalyticsStats(
        long totalCurrencies,
        long activeCurrencies,
        long inactiveCurrencies,
        String strongestCode,
        String weakestCode,
        double rateSpreadPercent,
        String mostVolatileCode,
        double mostVolatilePercent,
        double averageVolatilityPercent,
        String stalestCode,
        double stalestHours,
        List<String> rateBarLabels,
        List<Double> rateBarValues,
        List<String> volatilityLabels,
        List<Double> volatilityValues,
        List<String> freshnessLabels,
        List<Double> freshnessHours,
        List<String> trendLabels,
        List<String> trendCodes,
        List<List<Double>> trendSeries,
        List<String> converterCodes,
        Map<String, Double> converterRates
) {
}
