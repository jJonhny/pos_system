package com.example.pos_system.util;

import org.springframework.stereotype.Component;
import com.example.pos_system.entity.Currency;
import com.example.pos_system.service.CurrencyService;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import com.example.pos_system.entity.UnitType;

@Component("uiFormat")
public class UiFormat {
    private static final DateTimeFormatter DATE_TIME_FMT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
    private static final DateTimeFormatter DATE_FMT = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    private final CurrencyService currencyService;

    public UiFormat(CurrencyService currencyService) {
        this.currencyService = currencyService;
    }

    public String money(BigDecimal value) {
        if (value == null) return "-";
        Currency base = currencyService.getBaseCurrency();
        int decimals = base != null && base.getFractionDigits() != null ? base.getFractionDigits() : 2;
        BigDecimal scaled = value.setScale(decimals, RoundingMode.HALF_UP);
        String symbol = base != null ? base.getSymbol() : "$";
        String code = base != null ? base.getCode() : "USD";
        return formatCurrency(scaled, symbol, code, decimals);
    }

    public String moneyForCurrency(BigDecimal value, String currencyCode) {
        if (value == null) return "-";
        if (currencyCode == null || currencyCode.isBlank()) return money(value);
        Currency currency = currencyService.findByCode(currencyCode);
        if (currency == null) return value.toPlainString() + " " + currencyCode.toUpperCase();
        int decimals = currency.getFractionDigits() == null ? 2 : currency.getFractionDigits();
        BigDecimal scaled = value.setScale(decimals, RoundingMode.HALF_UP);
        return formatCurrency(scaled, currency.getSymbol(), currency.getCode(), decimals);
    }

    private String formatCurrency(BigDecimal value, String symbol, String code, int decimals) {
        String text = value.setScale(decimals, RoundingMode.HALF_UP).toPlainString();
        if (symbol != null && !symbol.isBlank()) {
            return symbol + text;
        }
        if (code != null && !code.isBlank()) {
            return text + " " + code;
        }
        return text;
    }

    public String dateTime(LocalDateTime value) {
        if (value == null) return "-";
        return value.format(DATE_TIME_FMT);
    }

    public String date(LocalDate value) {
        if (value == null) return "-";
        return value.format(DATE_FMT);
    }

    public String unitLabel(UnitType unitType) {
        if (unitType == null) return "pc";
        return switch (unitType) {
            case BOX -> "box";
            case CASE -> "case";
            default -> "pc";
        };
    }
}
