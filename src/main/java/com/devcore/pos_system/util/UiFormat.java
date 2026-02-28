package com.devcore.pos_system.util;

import org.springframework.stereotype.Component;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import com.devcore.pos_system.entity.PaymentMethod;
import com.devcore.pos_system.entity.Permission;
import com.devcore.pos_system.entity.PurchaseOrderStatus;
import com.devcore.pos_system.entity.SaleStatus;
import com.devcore.pos_system.entity.StockMovementType;
import com.devcore.pos_system.entity.SupplierStatus;
import com.devcore.pos_system.entity.UserRole;
import com.devcore.pos_system.modules.currency.application.CurrencyService;
import com.devcore.pos_system.modules.currency.domain.Currency;

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

import com.devcore.pos_system.entity.UnitType;

@Component("uiFormat")
public class UiFormat {
    private final CurrencyService currencyService;
    private final MessageSource messageSource;

    /**
     * Executes the UiFormat operation.
     * <p>Return value: A fully initialized UiFormat instance.</p>
     *
     * @param currencyService Parameter of type {@code CurrencyService} used by this operation.
     * @param messageSource Parameter of type {@code MessageSource} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public UiFormat(CurrencyService currencyService,
                    MessageSource messageSource) {
        this.currencyService = currencyService;
        this.messageSource = messageSource;
    }

    /**
     * Executes the money operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String money(BigDecimal value) {
        if (value == null) return "-";
        Currency base = currencyService.getBaseCurrency();
        int decimals = base != null && base.getFractionDigits() != null ? base.getFractionDigits() : 2;
        BigDecimal scaled = value.setScale(decimals, RoundingMode.HALF_UP);
        String symbol = base != null ? base.getSymbol() : "$";
        String code = base != null ? base.getCode() : "USD";
        return formatCurrency(scaled, symbol, code, decimals, LocaleContextHolder.getLocale());
    }

    /**
     * Executes the moneyForCurrency operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @param currencyCode Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String moneyForCurrency(BigDecimal value, String currencyCode) {
        if (value == null) return "-";
        if (currencyCode == null || currencyCode.isBlank()) return money(value);
        Currency currency = currencyService.findByCode(currencyCode);
        if (currency == null) return value.toPlainString() + " " + currencyCode.toUpperCase();
        int decimals = currency.getFractionDigits() == null ? 2 : currency.getFractionDigits();
        BigDecimal scaled = value.setScale(decimals, RoundingMode.HALF_UP);
        return formatCurrency(scaled, currency.getSymbol(), currency.getCode(), decimals, LocaleContextHolder.getLocale());
    }

    /**
     * Executes the formatCurrency operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @param symbol Parameter of type {@code String} used by this operation.
     * @param code Parameter of type {@code String} used by this operation.
     * @param decimals Parameter of type {@code int} used by this operation.
     * @param locale Parameter of type {@code Locale} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the dateTime operation.
     *
     * @param value Parameter of type {@code LocalDateTime} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String dateTime(LocalDateTime value) {
        if (value == null) return "-";
        Locale locale = LocaleContextHolder.getLocale();
        return value.format(DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM).withLocale(locale));
    }

    /**
     * Executes the date operation.
     *
     * @param value Parameter of type {@code LocalDate} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String date(LocalDate value) {
        if (value == null) return "-";
        Locale locale = LocaleContextHolder.getLocale();
        return value.format(DateTimeFormatter.ofLocalizedDate(FormatStyle.MEDIUM).withLocale(locale));
    }

    /**
     * Executes the unitLabel operation.
     *
     * @param unitType Parameter of type {@code UnitType} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String unitLabel(UnitType unitType) {
        Locale locale = LocaleContextHolder.getLocale();
        if (unitType == null) return messageSource.getMessage("common.unit.piece", null, "pc", locale);
        return switch (unitType) {
            case BOX -> messageSource.getMessage("common.unit.box", null, "box", locale);
            case CASE -> messageSource.getMessage("common.unit.case", null, "case", locale);
            default -> messageSource.getMessage("common.unit.piece", null, "pc", locale);
        };
    }

    /**
     * Executes the paymentMethodLabel operation.
     *
     * @param method Parameter of type {@code PaymentMethod} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    public String saleStatusLabel(SaleStatus status) {
        if (status == null) return "-";
        return enumLabel("sale.status.", status);
    }

    public String stockMovementTypeLabel(StockMovementType type) {
        if (type == null) return "-";
        return enumLabel("stock.movement.type.", type);
    }

    public String supplierStatusLabel(SupplierStatus status) {
        if (status == null) return "-";
        return enumLabel("supplier.status.", status);
    }

    public String purchaseOrderStatusLabel(PurchaseOrderStatus status) {
        if (status == null) return "-";
        return enumLabel("purchase.status.", status);
    }

    public String userRoleLabel(UserRole role) {
        if (role == null) return "-";
        return enumLabel("user.role.", role);
    }

    public String permissionLabel(Permission permission) {
        if (permission == null) return "-";
        return enumLabel("permission.", permission);
    }

    private String enumLabel(String keyPrefix, Enum<?> value) {
        Locale locale = LocaleContextHolder.getLocale();
        String fallback = value.name().replace('_', ' ');
        return messageSource.getMessage(keyPrefix + value.name().toLowerCase(Locale.ROOT), null, fallback, locale);
    }
}
