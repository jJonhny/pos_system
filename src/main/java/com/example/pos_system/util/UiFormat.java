package com.example.pos_system.util;

import org.springframework.stereotype.Component;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import com.example.pos_system.entity.Currency;
import com.example.pos_system.service.CurrencyService;
import com.example.pos_system.entity.PaymentMethod;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.FormatStyle;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

import com.example.pos_system.entity.UnitType;

@Component("uiFormat")
public class UiFormat {
    private final CurrencyService currencyService;
    private final MessageSource messageSource;

    public UiFormat(CurrencyService currencyService,
                    MessageSource messageSource) {
        this.currencyService = currencyService;
        this.messageSource = messageSource;
    }

    public String money(BigDecimal value) {
        if (value == null) return "-";
        Currency base = currencyService.getBaseCurrency();
        int decimals = base != null && base.getFractionDigits() != null ? base.getFractionDigits() : 2;
        BigDecimal scaled = value.setScale(decimals, RoundingMode.HALF_UP);
        String symbol = base != null ? base.getSymbol() : "$";
        String code = base != null ? base.getCode() : "USD";
        return formatCurrency(scaled, symbol, code, decimals, LocaleContextHolder.getLocale());
    }

    public String moneyForCurrency(BigDecimal value, String currencyCode) {
        if (value == null) return "-";
        if (currencyCode == null || currencyCode.isBlank()) return money(value);
        Currency currency = currencyService.findByCode(currencyCode);
        if (currency == null) return value.toPlainString() + " " + currencyCode.toUpperCase();
        int decimals = currency.getFractionDigits() == null ? 2 : currency.getFractionDigits();
        BigDecimal scaled = value.setScale(decimals, RoundingMode.HALF_UP);
        return formatCurrency(scaled, currency.getSymbol(), currency.getCode(), decimals, LocaleContextHolder.getLocale());
    }

    private String formatCurrency(BigDecimal value, String symbol, String code, int decimals, Locale locale) {
        NumberFormat nf = NumberFormat.getNumberInstance(locale == null ? Locale.ENGLISH : locale);
        if (nf instanceof DecimalFormat decimalFormat) {
            DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(locale == null ? Locale.ENGLISH : locale);
            decimalFormat.setDecimalFormatSymbols(symbols);
            decimalFormat.setGroupingUsed(true);
            decimalFormat.setMinimumFractionDigits(decimals);
            decimalFormat.setMaximumFractionDigits(decimals);
        }
        String text = nf.format(value.setScale(decimals, RoundingMode.HALF_UP));
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
        Locale locale = LocaleContextHolder.getLocale();
        return value.format(DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM).withLocale(locale));
    }

    public String date(LocalDate value) {
        if (value == null) return "-";
        Locale locale = LocaleContextHolder.getLocale();
        return value.format(DateTimeFormatter.ofLocalizedDate(FormatStyle.MEDIUM).withLocale(locale));
    }

    public String unitLabel(UnitType unitType) {
        Locale locale = LocaleContextHolder.getLocale();
        if (unitType == null) return messageSource.getMessage("common.unit.piece", null, "pc", locale);
        return switch (unitType) {
            case BOX -> messageSource.getMessage("common.unit.box", null, "box", locale);
            case CASE -> messageSource.getMessage("common.unit.case", null, "case", locale);
            default -> messageSource.getMessage("common.unit.piece", null, "pc", locale);
        };
    }

    public String paymentMethodLabel(PaymentMethod method) {
        Locale locale = LocaleContextHolder.getLocale();
        if (method == null) return messageSource.getMessage("payment.method.cash", null, "Cash", locale);
        return switch (method) {
            case CASH -> messageSource.getMessage("payment.method.cash", null, "Cash", locale);
            case CARD -> messageSource.getMessage("payment.method.card", null, "Card", locale);
            case QR -> messageSource.getMessage("payment.method.qr", null, "QR", locale);
            case MIXED -> messageSource.getMessage("payment.method.mixed", null, "Mixed", locale);
        };
    }
}
