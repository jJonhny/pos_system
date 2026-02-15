package com.example.pos_system.service;

import com.example.pos_system.entity.Currency;
import com.example.pos_system.entity.PaymentMethod;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SalePayment;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;

@Service
public class ReceiptPaymentService {
    private final CurrencyService currencyService;

    public ReceiptPaymentService(CurrencyService currencyService) {
        this.currencyService = currencyService;
    }

    public List<ReceiptPaymentLine> buildLines(Sale sale) {
        List<ReceiptPaymentLine> lines = new ArrayList<>();
        if (sale == null) return lines;

        List<SalePayment> payments = sale.getPayments();
        if (payments == null || payments.isEmpty()) {
            PaymentMethod method = sale.getPaymentMethod() == null ? PaymentMethod.CASH : sale.getPaymentMethod();
            BigDecimal total = money(sale.getTotal());
            if (method == PaymentMethod.CASH) {
                lines.add(cashLine(method, total, null, BigDecimal.ONE, total, true));
            } else {
                lines.add(nonCashLine(method, total, null, null));
            }
            return lines;
        }

        boolean singleCashPayment = payments.size() == 1
                && payments.getFirst() != null
                && payments.getFirst().getMethod() == PaymentMethod.CASH;
        BigDecimal saleTotal = money(sale.getTotal());

        for (SalePayment payment : payments) {
            if (payment == null) continue;
            PaymentMethod method = payment.getMethod() == null ? PaymentMethod.CASH : payment.getMethod();
            BigDecimal amountBase = money(payment.getAmount());
            String currencyCode = blankToNull(payment.getCurrencyCode());
            BigDecimal rate = positive(payment.getCurrencyRate(), resolveRate(currencyCode));
            BigDecimal foreignAmount = payment.getForeignAmount();

            if (method == PaymentMethod.CASH) {
                BigDecimal dueBase = singleCashPayment ? saleTotal : amountBase;
                BigDecimal receivedForeign = positive(foreignAmount, null);
                if (receivedForeign == null) {
                    receivedForeign = rate.compareTo(BigDecimal.ZERO) > 0
                            ? dueBase.divide(rate, 4, RoundingMode.HALF_UP)
                            : dueBase;
                }
                lines.add(cashLine(method, dueBase, currencyCode, rate, receivedForeign, singleCashPayment));
            } else {
                lines.add(nonCashLine(method, amountBase, currencyCode, positive(foreignAmount, null)));
            }
        }
        return lines;
    }

    public BigDecimal totalCashReceivedBase(List<ReceiptPaymentLine> lines) {
        if (lines == null || lines.isEmpty()) return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal total = BigDecimal.ZERO;
        for (ReceiptPaymentLine line : lines) {
            if (line == null || line.cashReceivedBase() == null) continue;
            total = total.add(line.cashReceivedBase());
        }
        return total.setScale(2, RoundingMode.HALF_UP);
    }

    public BigDecimal totalCashChangeBase(List<ReceiptPaymentLine> lines) {
        if (lines == null || lines.isEmpty()) return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal total = BigDecimal.ZERO;
        for (ReceiptPaymentLine line : lines) {
            if (line == null || line.cashChangeBase() == null) continue;
            total = total.add(line.cashChangeBase());
        }
        return total.setScale(2, RoundingMode.HALF_UP);
    }

    private ReceiptPaymentLine cashLine(PaymentMethod method,
                                        BigDecimal amountBase,
                                        String currencyCode,
                                        BigDecimal rate,
                                        BigDecimal receivedForeign,
                                        boolean singleCashPayment) {
        BigDecimal dueBase = money(amountBase);
        BigDecimal safeRate = positive(rate, BigDecimal.ONE);
        BigDecimal safeReceivedForeign = money4(positive(receivedForeign, BigDecimal.ZERO));
        BigDecimal dueForeign = safeRate.compareTo(BigDecimal.ZERO) > 0
                ? dueBase.divide(safeRate, 4, RoundingMode.HALF_UP)
                : dueBase.setScale(4, RoundingMode.HALF_UP);
        BigDecimal receivedBase = safeReceivedForeign.multiply(safeRate).setScale(2, RoundingMode.HALF_UP);
        BigDecimal changeBase = receivedBase.subtract(dueBase);
        if (changeBase.compareTo(BigDecimal.ZERO) < 0) changeBase = BigDecimal.ZERO;
        changeBase = changeBase.setScale(2, RoundingMode.HALF_UP);
        BigDecimal changeForeign = safeRate.compareTo(BigDecimal.ZERO) > 0
                ? changeBase.divide(safeRate, 4, RoundingMode.HALF_UP)
                : BigDecimal.ZERO.setScale(4, RoundingMode.HALF_UP);
        boolean displayChange = singleCashPayment || changeBase.compareTo(BigDecimal.ZERO) > 0;
        return new ReceiptPaymentLine(
                method,
                dueBase,
                currencyCode,
                safeRate,
                dueForeign,
                receivedBase,
                safeReceivedForeign,
                changeBase,
                changeForeign,
                displayChange
        );
    }

    private ReceiptPaymentLine nonCashLine(PaymentMethod method,
                                           BigDecimal amountBase,
                                           String currencyCode,
                                           BigDecimal foreignAmount) {
        return new ReceiptPaymentLine(
                method,
                money(amountBase),
                currencyCode,
                null,
                foreignAmount == null ? null : money4(foreignAmount),
                null,
                null,
                null,
                null,
                false
        );
    }

    private BigDecimal money(BigDecimal value) {
        if (value == null) return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        return value.setScale(2, RoundingMode.HALF_UP);
    }

    private BigDecimal money4(BigDecimal value) {
        if (value == null) return BigDecimal.ZERO.setScale(4, RoundingMode.HALF_UP);
        return value.setScale(4, RoundingMode.HALF_UP);
    }

    private BigDecimal positive(BigDecimal value, BigDecimal fallback) {
        if (value == null) return fallback;
        return value.compareTo(BigDecimal.ZERO) > 0 ? value : fallback;
    }

    private BigDecimal resolveRate(String currencyCode) {
        if (currencyCode == null || currencyCode.isBlank()) return BigDecimal.ONE;
        Currency currency = currencyService.findByCode(currencyCode);
        if (currency == null || currency.getRateToBase() == null || currency.getRateToBase().compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ONE;
        }
        return currency.getRateToBase();
    }

    private String blankToNull(String value) {
        if (value == null) return null;
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed.toUpperCase();
    }

    public record ReceiptPaymentLine(
            PaymentMethod method,
            BigDecimal amountBase,
            String currencyCode,
            BigDecimal currencyRate,
            BigDecimal foreignAmount,
            BigDecimal cashReceivedBase,
            BigDecimal cashReceivedForeign,
            BigDecimal cashChangeBase,
            BigDecimal cashChangeForeign,
            boolean displayCashChange
    ) {
        public boolean cash() {
            return method == PaymentMethod.CASH;
        }
    }
}
