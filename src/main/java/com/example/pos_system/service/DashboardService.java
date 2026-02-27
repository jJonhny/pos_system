package com.example.pos_system.service;

import com.example.pos_system.dto.DashboardStats;
import com.example.pos_system.dto.MoverStat;
import com.example.pos_system.dto.CashierPerformance;
import com.example.pos_system.dto.CategoryPerformance;
import com.example.pos_system.dto.CustomerRfm;
import com.example.pos_system.dto.ReorderRecommendation;
import com.example.pos_system.dto.ShiftPerformance;
import com.example.pos_system.dto.SimpleStat;
import com.example.pos_system.dto.SkuPerformance;
import com.example.pos_system.entity.PaymentMethod;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SaleItem;
import com.example.pos_system.entity.SaleStatus;
import com.example.pos_system.entity.Shift;
import com.example.pos_system.entity.ShiftStatus;
import com.example.pos_system.modules.currency.application.CurrencyService;
import com.example.pos_system.modules.currency.domain.Currency;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.SaleRepo;
import com.example.pos_system.repository.ShiftRepo;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Transactional(readOnly = true)
public class DashboardService {
    private static final int DAILY_DAYS = 7;
    private static final int LOW_STOCK_DAYS = 14;
    private static final int TURNOVER_DAYS = 30;
    private static final int MONTHS = 12;
    private static final int TOP_N = 5;

    private static final DateTimeFormatter DAY_FMT = DateTimeFormatter.ofPattern("MMM d", Locale.ENGLISH);
    private static final DateTimeFormatter MONTH_FMT = DateTimeFormatter.ofPattern("MMM yy", Locale.ENGLISH);

    private final SaleRepo saleRepo;
    private final ProductRepo productRepo;
    private final ShiftRepo shiftRepo;
    private final CurrencyService currencyService;

    /**
     * Executes the DashboardService operation.
     * <p>Return value: A fully initialized DashboardService instance.</p>
     *
     * @param saleRepo Parameter of type {@code SaleRepo} used by this operation.
     * @param productRepo Parameter of type {@code ProductRepo} used by this operation.
     * @param shiftRepo Parameter of type {@code ShiftRepo} used by this operation.
     * @param currencyService Parameter of type {@code CurrencyService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public DashboardService(SaleRepo saleRepo, ProductRepo productRepo, ShiftRepo shiftRepo, CurrencyService currencyService) {
        this.saleRepo = saleRepo;
        this.productRepo = productRepo;
        this.shiftRepo = shiftRepo;
        this.currencyService = currencyService;
    }

    /**
     * Executes the buildStats operation.
     *
     * @return {@code DashboardStats} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public DashboardStats buildStats() {
        List<Sale> sales = saleRepo.findAll();
        List<Product> products = productRepo.findAll();

        List<LocalDate> dailyRange = buildDailyRange(DAILY_DAYS);
        Map<LocalDate, BigDecimal> dailyTotals = new LinkedHashMap<>();
        for (LocalDate d : dailyRange) {
            dailyTotals.put(d, BigDecimal.ZERO);
        }

        List<YearMonth> monthlyRange = buildMonthlyRange();
        Map<YearMonth, BigDecimal> monthlyTotals = new LinkedHashMap<>();
        for (YearMonth ym : monthlyRange) {
            monthlyTotals.put(ym, BigDecimal.ZERO);
        }

        Map<LocalDate, EnumMap<PaymentMethod, BigDecimal>> paymentTotals = new LinkedHashMap<>();
        for (LocalDate d : dailyRange) {
            EnumMap<PaymentMethod, BigDecimal> methodMap = new EnumMap<>(PaymentMethod.class);
            for (PaymentMethod method : PaymentMethod.values()) {
                methodMap.put(method, BigDecimal.ZERO);
            }
            paymentTotals.put(d, methodMap);
        }

        Map<LocalDate, BigDecimal> grossRevenueDaily = new LinkedHashMap<>();
        Map<LocalDate, BigDecimal> grossCostDaily = new LinkedHashMap<>();
        for (LocalDate d : dailyRange) {
            grossRevenueDaily.put(d, BigDecimal.ZERO);
            grossCostDaily.put(d, BigDecimal.ZERO);
        }

        List<LocalDate> lowStockRange = buildDailyRange(LOW_STOCK_DAYS);
        Map<LocalDate, Integer> lowStockIndex = new LinkedHashMap<>();
        for (int i = 0; i < lowStockRange.size(); i++) {
            lowStockIndex.put(lowStockRange.get(i), i);
        }

        Map<Long, int[]> productSalesByDay = new LinkedHashMap<>();
        for (Product p : products) {
            if (p.getLowStockThreshold() != null && p.getStockQty() != null) {
                productSalesByDay.put(p.getId(), new int[lowStockRange.size()]);
            }
        }

        Map<String, BigDecimal> productTotals = new LinkedHashMap<>();
        Map<String, BigDecimal> categoryTotals = new LinkedHashMap<>();
        Map<String, Integer> productQtyTotals = new LinkedHashMap<>();
        Map<Long, Integer> productQty30d = new LinkedHashMap<>();
        Map<String, BigDecimal> currencyTotals = new LinkedHashMap<>();

        String baseCurrencyCode = "BASE";
        Currency baseCurrency = currencyService.getBaseCurrency();
        if (baseCurrency != null && baseCurrency.getCode() != null && !baseCurrency.getCode().isBlank()) {
            baseCurrencyCode = baseCurrency.getCode().trim().toUpperCase(Locale.ENGLISH);
        }

        int[][] heatmap = new int[7][24];
        long voidCount = 0;
        BigDecimal voidTotal = BigDecimal.ZERO;

        long saleCount = 0;
        long refundCount = 0;
        long discountCount = 0;
        long splitPaymentCount = 0;
        int totalItems = 0;
        int suspiciousDiscountCount = 0;
        int highRefundCount = 0;
        int invoiceIssueCount = 0;

        BigDecimal netRevenueTotal = BigDecimal.ZERO;
        BigDecimal discountTotal = BigDecimal.ZERO;
        BigDecimal taxTotal = BigDecimal.ZERO;
        BigDecimal refundTotal = BigDecimal.ZERO;
        BigDecimal discountRevenue = BigDecimal.ZERO;
        BigDecimal discountProfit = BigDecimal.ZERO;
        BigDecimal nonDiscountRevenue = BigDecimal.ZERO;
        BigDecimal nonDiscountProfit = BigDecimal.ZERO;

        BigDecimal[] dayRevenue = new BigDecimal[7];
        BigDecimal[] hourRevenue = new BigDecimal[24];
        for (int i = 0; i < 7; i++) dayRevenue[i] = BigDecimal.ZERO;
        for (int i = 0; i < 24; i++) hourRevenue[i] = BigDecimal.ZERO;

        Map<String, CashierAccumulator> cashierStats = new LinkedHashMap<>();
        Map<String, RevenueCost> skuRevenueCost = new LinkedHashMap<>();
        Map<String, RevenueCost> categoryRevenueCost = new LinkedHashMap<>();
        Map<Long, CustomerAccumulator> customerStats = new LinkedHashMap<>();
        Map<Long, Integer> customerRefunds = new HashMap<>();

        LocalDate turnoverStart = LocalDate.now().minusDays(TURNOVER_DAYS - 1);

        for (Sale sale : sales) {
            if (sale.getCreatedAt() == null) continue;

            if (sale.getStatus() == SaleStatus.VOID) {
                voidCount++;
                voidTotal = voidTotal.add(safeAmount(sale.getTotal()));
                continue;
            }
            if (sale.getStatus() != null
                    && sale.getStatus() != SaleStatus.PAID
                    && sale.getStatus() != SaleStatus.PARTIALLY_RETURNED
                    && sale.getStatus() != SaleStatus.RETURNED) {
                continue;
            }

            LocalDate saleDate = sale.getCreatedAt().toLocalDate();
            BigDecimal saleTotal = safeAmount(sale.getTotal()).subtract(safeAmount(sale.getRefundedTotal()));
            if (saleTotal.compareTo(BigDecimal.ZERO) < 0) saleTotal = BigDecimal.ZERO;

            saleCount++;
            netRevenueTotal = netRevenueTotal.add(saleTotal);
            taxTotal = taxTotal.add(safeAmount(sale.getTax()));
            discountTotal = discountTotal.add(safeAmount(sale.getDiscount()));
            if (safeAmount(sale.getDiscount()).compareTo(BigDecimal.ZERO) > 0) discountCount++;
            if (safeAmount(sale.getRefundedTotal()).compareTo(BigDecimal.ZERO) > 0) {
                refundCount++;
                refundTotal = refundTotal.add(safeAmount(sale.getRefundedTotal()));
                if (safeAmount(sale.getRefundedTotal()).compareTo(new BigDecimal("100")) > 0) {
                    highRefundCount++;
                }
            }
            if (sale.getPayments() != null && sale.getPayments().size() > 1) {
                splitPaymentCount++;
            } else if (sale.getPaymentMethod() == PaymentMethod.MIXED) {
                splitPaymentCount++;
            }

            if (sale.getSubtotal() == null || sale.getTotal() == null) {
                invoiceIssueCount++;
            }

            if (dailyTotals.containsKey(saleDate)) {
                dailyTotals.put(saleDate, dailyTotals.get(saleDate).add(saleTotal));
                EnumMap<PaymentMethod, BigDecimal> map = paymentTotals.get(saleDate);
                if (sale.getPayments() != null && !sale.getPayments().isEmpty()) {
                    for (var payment : sale.getPayments()) {
                        if (payment.getMethod() == null || payment.getAmount() == null) continue;
                        map.put(payment.getMethod(), map.get(payment.getMethod()).add(payment.getAmount()));
                    }
                } else if (sale.getPaymentMethod() != null) {
                    map.put(sale.getPaymentMethod(), map.get(sale.getPaymentMethod()).add(saleTotal));
                }
            }

            if (sale.getPayments() != null && !sale.getPayments().isEmpty()) {
                for (var payment : sale.getPayments()) {
                    if (payment.getAmount() == null) continue;
                    String code = payment.getCurrencyCode();
                    if (code == null || code.isBlank()) {
                        code = baseCurrencyCode;
                    } else {
                        code = code.trim().toUpperCase(Locale.ENGLISH);
                    }
                    currencyTotals.put(code, currencyTotals.getOrDefault(code, BigDecimal.ZERO).add(payment.getAmount()));
                }
            } else if (saleTotal.compareTo(BigDecimal.ZERO) > 0) {
                currencyTotals.put(baseCurrencyCode,
                        currencyTotals.getOrDefault(baseCurrencyCode, BigDecimal.ZERO).add(saleTotal));
            }

            YearMonth saleMonth = YearMonth.from(saleDate);
            if (monthlyTotals.containsKey(saleMonth)) {
                monthlyTotals.put(saleMonth, monthlyTotals.get(saleMonth).add(saleTotal));
            }

            int dayIdx = sale.getCreatedAt().getDayOfWeek().getValue() - 1;
            int hourIdx = sale.getCreatedAt().getHour();
            if (dayIdx >= 0 && dayIdx < 7 && hourIdx >= 0 && hourIdx < 24) {
                heatmap[dayIdx][hourIdx] += 1;
                dayRevenue[dayIdx] = dayRevenue[dayIdx].add(saleTotal);
                hourRevenue[hourIdx] = hourRevenue[hourIdx].add(saleTotal);
            }

            BigDecimal saleProfit = BigDecimal.ZERO;
            int saleItems = 0;
            if (sale.getItems() != null) {
                for (SaleItem item : sale.getItems()) {
                    BigDecimal lineTotal = lineTotal(item);
                    String productName = "Unknown";
                    String categoryName = "Uncategorized";
                    Product product = item.getProduct();
                    if (product != null) {
                        if (product.getName() != null) productName = product.getName();
                        if (product.getCategory() != null && product.getCategory().getName() != null) {
                            categoryName = product.getCategory().getName();
                        }
                    }
                    productTotals.put(productName, productTotals.getOrDefault(productName, BigDecimal.ZERO).add(lineTotal));
                    categoryTotals.put(categoryName, categoryTotals.getOrDefault(categoryName, BigDecimal.ZERO).add(lineTotal));

                    int qty = item.getQty() == null ? 0 : item.getQty();
                    int returned = item.getReturnedQty() == null ? 0 : item.getReturnedQty();
                    int effectiveUnits = Math.max(0, qty - returned);
                    int unitSize = unitSize(item);
                    int effectivePieces = effectiveUnits * unitSize;
                    productQtyTotals.put(productName, productQtyTotals.getOrDefault(productName, 0) + effectivePieces);
                    saleItems += effectivePieces;

                    if (product != null && saleDate.isAfter(turnoverStart.minusDays(1))) {
                        productQty30d.put(product.getId(), productQty30d.getOrDefault(product.getId(), 0) + effectivePieces);
                    }

                    if (product != null && lowStockIndex.containsKey(saleDate) && productSalesByDay.containsKey(product.getId())) {
                        int idx = lowStockIndex.get(saleDate);
                        productSalesByDay.get(product.getId())[idx] += effectivePieces;
                    }

                    BigDecimal costPrice = product != null && product.getCostPrice() != null
                            ? product.getCostPrice()
                            : BigDecimal.ZERO;
                    BigDecimal cost = costPrice.multiply(BigDecimal.valueOf(effectivePieces));
                    saleProfit = saleProfit.add(lineTotal.subtract(cost));

                    RevenueCost skuRc = skuRevenueCost.getOrDefault(productName, new RevenueCost());
                    skuRc.revenue = skuRc.revenue.add(lineTotal);
                    skuRc.cost = skuRc.cost.add(cost);
                    skuRc.qty += effectivePieces;
                    skuRevenueCost.put(productName, skuRc);

                    RevenueCost catRc = categoryRevenueCost.getOrDefault(categoryName, new RevenueCost());
                    catRc.revenue = catRc.revenue.add(lineTotal);
                    catRc.cost = catRc.cost.add(cost);
                    categoryRevenueCost.put(categoryName, catRc);

                    if (grossRevenueDaily.containsKey(saleDate)) {
                        grossRevenueDaily.put(saleDate, grossRevenueDaily.get(saleDate).add(lineTotal));
                        grossCostDaily.put(saleDate, grossCostDaily.get(saleDate).add(cost));
                    }
                }
            }

            totalItems += saleItems;

            if (safeAmount(sale.getDiscount()).compareTo(BigDecimal.ZERO) > 0) {
                discountRevenue = discountRevenue.add(saleTotal);
                discountProfit = discountProfit.add(saleProfit);
                BigDecimal subtotal = safeAmount(sale.getSubtotal());
                if (subtotal.compareTo(BigDecimal.ZERO) > 0) {
                    BigDecimal discountRatio = safeAmount(sale.getDiscount()).divide(subtotal, 4, RoundingMode.HALF_UP);
                    if (discountRatio.compareTo(new BigDecimal("0.30")) > 0) {
                        suspiciousDiscountCount++;
                    }
                }
            } else {
                nonDiscountRevenue = nonDiscountRevenue.add(saleTotal);
                nonDiscountProfit = nonDiscountProfit.add(saleProfit);
            }

            String cashier = sale.getCashierUsername() == null ? "Unknown" : sale.getCashierUsername();
            CashierAccumulator cashierAcc = cashierStats.getOrDefault(cashier, new CashierAccumulator());
            cashierAcc.revenue = cashierAcc.revenue.add(saleTotal);
            cashierAcc.transactions += 1;
            cashierAcc.items += saleItems;
            cashierAcc.hours.add(saleDate + "-" + hourIdx);
            cashierStats.put(cashier, cashierAcc);

            if (sale.getCustomer() != null && sale.getCustomer().getId() != null) {
                Long customerId = sale.getCustomer().getId();
                CustomerAccumulator custAcc = customerStats.getOrDefault(customerId,
                        new CustomerAccumulator(sale.getCustomer().getName()));
                custAcc.frequency += 1;
                custAcc.monetary = custAcc.monetary.add(saleTotal);
                if (custAcc.lastPurchase == null || sale.getCreatedAt().isAfter(custAcc.lastPurchase)) {
                    custAcc.lastPurchase = sale.getCreatedAt();
                }
                customerStats.put(customerId, custAcc);
                if (safeAmount(sale.getRefundedTotal()).compareTo(BigDecimal.ZERO) > 0) {
                    customerRefunds.put(customerId, customerRefunds.getOrDefault(customerId, 0) + 1);
                }
            }
        }

        Map<String, Integer> lowStockCategoryCounts = new LinkedHashMap<>();
        for (Product p : products) {
            if (!p.isLowStock()) continue;
            String categoryName = "Uncategorized";
            if (p.getCategory() != null && p.getCategory().getName() != null) {
                categoryName = p.getCategory().getName();
            }
            lowStockCategoryCounts.put(categoryName, lowStockCategoryCounts.getOrDefault(categoryName, 0) + 1);
        }

        int[] lowStockCounts = new int[lowStockRange.size()];
        for (Product p : products) {
            if (p.getStockQty() == null || p.getLowStockThreshold() == null) continue;
            int[] salesByDay = productSalesByDay.getOrDefault(p.getId(), new int[lowStockRange.size()]);
            int runningAfter = 0;
            for (int i = lowStockRange.size() - 1; i >= 0; i--) {
                int stockAtDay = p.getStockQty() + runningAfter;
                if (stockAtDay <= p.getLowStockThreshold()) {
                    lowStockCounts[i] += 1;
                }
                runningAfter += salesByDay[i];
            }
        }

        List<String> dailyLabels = dailyRange.stream().map(d -> d.format(DAY_FMT)).toList();
        List<Double> dailySales = dailyRange.stream().map(d -> toDouble(dailyTotals.get(d))).toList();

        List<String> monthlyLabels = monthlyRange.stream().map(m -> m.format(MONTH_FMT)).toList();
        List<Double> monthlySales = monthlyRange.stream().map(m -> toDouble(monthlyTotals.get(m))).toList();

        List<Map.Entry<String, BigDecimal>> topProducts = topEntries(productTotals, TOP_N);
        List<String> topProductLabels = topProducts.stream().map(Map.Entry::getKey).toList();
        List<Double> topProductValues = topProducts.stream().map(e -> toDouble(e.getValue())).toList();

        List<Map.Entry<String, BigDecimal>> topCategories = topEntries(categoryTotals, TOP_N);
        List<String> topCategoryLabels = topCategories.stream().map(Map.Entry::getKey).toList();
        List<Double> topCategoryValues = topCategories.stream().map(e -> toDouble(e.getValue())).toList();

        List<Map.Entry<String, Integer>> topQty = topEntriesInt(productQtyTotals, TOP_N);
        List<String> topQtyLabels = topQty.stream().map(Map.Entry::getKey).toList();
        List<Double> topQtyValues = topQty.stream().map(e -> e.getValue().doubleValue()).toList();

        List<Double> paymentCash = new ArrayList<>();
        List<Double> paymentCard = new ArrayList<>();
        List<Double> paymentQr = new ArrayList<>();
        for (LocalDate d : dailyRange) {
            EnumMap<PaymentMethod, BigDecimal> map = paymentTotals.get(d);
            paymentCash.add(toDouble(map.get(PaymentMethod.CASH)));
            paymentCard.add(toDouble(map.get(PaymentMethod.CARD)));
            paymentQr.add(toDouble(map.get(PaymentMethod.QR)));
        }

        RevenueShare revenueShare = buildRevenueShare(categoryTotals, TOP_N);
        RevenueShare currencyShare = buildRevenueShare(currencyTotals, TOP_N);

        List<Double> grossRevenueValues = dailyRange.stream().map(d -> toDouble(grossRevenueDaily.get(d))).toList();
        List<Double> grossCostValues = dailyRange.stream().map(d -> toDouble(grossCostDaily.get(d))).toList();
        List<Double> grossProfitValues = new ArrayList<>();
        BigDecimal grossRevenueTotal = BigDecimal.ZERO;
        BigDecimal grossProfitTotal = BigDecimal.ZERO;
        for (int i = 0; i < dailyRange.size(); i++) {
            BigDecimal revenue = grossRevenueDaily.get(dailyRange.get(i));
            BigDecimal cost = grossCostDaily.get(dailyRange.get(i));
            BigDecimal profit = revenue.subtract(cost);
            grossRevenueTotal = grossRevenueTotal.add(revenue);
            grossProfitTotal = grossProfitTotal.add(profit);
            grossProfitValues.add(toDouble(profit));
        }
        double grossMarginPercent = grossRevenueTotal.compareTo(BigDecimal.ZERO) > 0
                ? grossProfitTotal.divide(grossRevenueTotal, 4, RoundingMode.HALF_UP).multiply(new BigDecimal("100")).doubleValue()
                : 0.0;

        List<String> lowStockTrendLabels = lowStockRange.stream().map(d -> d.format(DAY_FMT)).toList();
        List<Double> lowStockTrendCounts = new ArrayList<>();
        for (int i = 0; i < lowStockCounts.length; i++) {
            lowStockTrendCounts.add((double) lowStockCounts[i]);
        }

        List<Map.Entry<String, Integer>> lowStockByCategory = lowStockCategoryCounts.entrySet().stream()
                .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                .collect(Collectors.toList());
        List<String> lowStockCategoryLabels = lowStockByCategory.stream().map(Map.Entry::getKey).toList();
        List<Double> lowStockCategoryValues = lowStockByCategory.stream()
                .map(e -> e.getValue().doubleValue())
                .toList();

        List<String> heatmapDays = List.of("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun");
        List<Integer> heatmapHours = new ArrayList<>();
        for (int h = 0; h < 24; h++) heatmapHours.add(h);
        List<List<Integer>> heatmapValues = new ArrayList<>();
        int heatmapMax = 0;
        for (int d = 0; d < 7; d++) {
            List<Integer> row = new ArrayList<>();
            for (int h = 0; h < 24; h++) {
                int val = heatmap[d][h];
                heatmapMax = Math.max(heatmapMax, val);
                row.add(val);
            }
            heatmapValues.add(row);
        }

        List<MoverStat> moverStats = new ArrayList<>();
        for (Product p : products) {
            int qty = productQty30d.getOrDefault(p.getId(), 0);
            Double daysOfStock = null;
            if (p.getStockQty() != null) {
                double avgDaily = qty / (double) TURNOVER_DAYS;
                if (avgDaily > 0) {
                    daysOfStock = p.getStockQty() / avgDaily;
                }
            }
            moverStats.add(new MoverStat(
                    p.getName() == null ? "Unknown" : p.getName(),
                    qty,
                    p.getStockQty(),
                    daysOfStock
            ));
        }

        List<MoverStat> fastMovers = moverStats.stream()
                .sorted(Comparator.comparingInt(MoverStat::qtySold).reversed())
                .limit(TOP_N)
                .collect(Collectors.toList());
        List<MoverStat> slowMovers = moverStats.stream()
                .sorted(Comparator.comparingInt(MoverStat::qtySold))
                .limit(TOP_N)
                .collect(Collectors.toList());

        double avgOrderValue = saleCount > 0
                ? netRevenueTotal.divide(BigDecimal.valueOf(saleCount), 4, RoundingMode.HALF_UP).doubleValue()
                : 0.0;
        double avgItemsPerSale = saleCount > 0
                ? totalItems / (double) saleCount
                : 0.0;
        double discountRatePercent = saleCount > 0 ? (discountCount * 100.0 / saleCount) : 0.0;
        double splitPaymentRatePercent = saleCount > 0 ? (splitPaymentCount * 100.0 / saleCount) : 0.0;

        List<CashierPerformance> cashierPerformance = cashierStats.entrySet().stream()
                .map(entry -> {
                    CashierAccumulator acc = entry.getValue();
                    double revenue = acc.revenue.doubleValue();
                    double avgOrder = acc.transactions > 0 ? revenue / acc.transactions : 0.0;
                    double itemsPerHour = acc.hours.isEmpty() ? 0.0 : acc.items / (double) acc.hours.size();
                    double itemsPerMinute = itemsPerHour / 60.0;
                    return new CashierPerformance(entry.getKey(), revenue, acc.transactions, acc.items, avgOrder, itemsPerMinute);
                })
                .sorted(Comparator.comparingDouble(CashierPerformance::revenue).reversed())
                .limit(TOP_N)
                .collect(Collectors.toList());

        List<String> dayOfWeekLabels = List.of("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun");
        List<Double> dayOfWeekRevenue = new ArrayList<>();
        for (int i = 0; i < dayRevenue.length; i++) {
            dayOfWeekRevenue.add(toDouble(dayRevenue[i]));
        }
        List<String> hourLabels = new ArrayList<>();
        List<Double> hourRevenueValues = new ArrayList<>();
        for (int h = 0; h < 24; h++) {
            hourLabels.add(String.valueOf(h));
            hourRevenueValues.add(toDouble(hourRevenue[h]));
        }

        List<SkuPerformance> skuPerformance = skuRevenueCost.entrySet().stream()
                .map(entry -> {
                    RevenueCost rc = entry.getValue();
                    BigDecimal profit = rc.revenue.subtract(rc.cost);
                    double margin = rc.revenue.compareTo(BigDecimal.ZERO) > 0
                            ? profit.divide(rc.revenue, 4, RoundingMode.HALF_UP).multiply(new BigDecimal("100")).doubleValue()
                            : 0.0;
                    return new SkuPerformance(entry.getKey(), toDouble(rc.revenue), rc.qty, toDouble(profit), margin);
                })
                .sorted(Comparator.comparingDouble(SkuPerformance::revenue).reversed())
                .limit(TOP_N)
                .collect(Collectors.toList());

        List<CategoryPerformance> categoryPerformance = categoryRevenueCost.entrySet().stream()
                .map(entry -> {
                    RevenueCost rc = entry.getValue();
                    BigDecimal profit = rc.revenue.subtract(rc.cost);
                    double margin = rc.revenue.compareTo(BigDecimal.ZERO) > 0
                            ? profit.divide(rc.revenue, 4, RoundingMode.HALF_UP).multiply(new BigDecimal("100")).doubleValue()
                            : 0.0;
                    return new CategoryPerformance(entry.getKey(), toDouble(rc.revenue), toDouble(profit), margin);
                })
                .sorted(Comparator.comparingDouble(CategoryPerformance::revenue).reversed())
                .limit(TOP_N)
                .collect(Collectors.toList());

        double discountMarginPercent = discountRevenue.compareTo(BigDecimal.ZERO) > 0
                ? discountProfit.divide(discountRevenue, 4, RoundingMode.HALF_UP).multiply(new BigDecimal("100")).doubleValue()
                : 0.0;
        double nonDiscountMarginPercent = nonDiscountRevenue.compareTo(BigDecimal.ZERO) > 0
                ? nonDiscountProfit.divide(nonDiscountRevenue, 4, RoundingMode.HALF_UP).multiply(new BigDecimal("100")).doubleValue()
                : 0.0;

        BigDecimal deadStockCost = BigDecimal.ZERO;
        long outOfStockCount = 0;
        long lowStockCount = 0;
        int totalSold30d = 0;
        int totalStock = 0;
        List<ReorderRecommendation> reorderRecommendations = new ArrayList<>();
        for (Product p : products) {
            int sold30d = productQty30d.getOrDefault(p.getId(), 0);
            totalSold30d += sold30d;
            if (p.getStockQty() != null) {
                totalStock += p.getStockQty();
                if (p.getStockQty() == 0) outOfStockCount++;
            }
            if (p.isLowStock()) lowStockCount++;
            if (sold30d == 0 && p.getCostPrice() != null && p.getStockQty() != null) {
                deadStockCost = deadStockCost.add(p.getCostPrice().multiply(BigDecimal.valueOf(p.getStockQty())));
            }
            if (p.getStockQty() != null) {
                double avgDaily = sold30d / (double) TURNOVER_DAYS;
                if (avgDaily > 0) {
                    double daysOfStock = p.getStockQty() / avgDaily;
                    boolean low = p.isLowStock();
                    if (daysOfStock <= 7 || low) {
                        reorderRecommendations.add(new ReorderRecommendation(
                                p.getName() == null ? "Unknown" : p.getName(),
                                p.getStockQty(),
                                avgDaily,
                                daysOfStock,
                                low
                        ));
                    }
                }
            }
        }
        double sellThroughPercent = (totalSold30d + totalStock) > 0
                ? (totalSold30d * 100.0 / (totalSold30d + totalStock))
                : 0.0;
        reorderRecommendations = reorderRecommendations.stream()
                .sorted(Comparator.comparingDouble(ReorderRecommendation::daysOfStock))
                .limit(TOP_N)
                .collect(Collectors.toList());

        long newCustomerCount = 0;
        long returningCustomerCount = 0;
        List<CustomerRfm> customerRfm = new ArrayList<>();
        List<CustomerRfm> customerClv = new ArrayList<>();
        LocalDate today = LocalDate.now();
        for (CustomerAccumulator cust : customerStats.values()) {
            if (cust.frequency <= 1) newCustomerCount++;
            else returningCustomerCount++;
            int recencyDays = cust.lastPurchase == null ? 999 : (int) java.time.temporal.ChronoUnit.DAYS.between(cust.lastPurchase.toLocalDate(), today);
            int score = rfmScore(recencyDays, cust.frequency, cust.monetary);
            CustomerRfm rfm = new CustomerRfm(
                    cust.name == null ? "Customer" : cust.name,
                    recencyDays,
                    cust.frequency,
                    toDouble(cust.monetary),
                    score
            );
            customerRfm.add(rfm);
            customerClv.add(rfm);
        }
        List<CustomerRfm> topCustomerRfm = customerRfm.stream()
                .sorted(Comparator.comparingInt(CustomerRfm::score).reversed()
                        .thenComparingDouble(CustomerRfm::monetary).reversed())
                .limit(TOP_N)
                .collect(Collectors.toList());
        List<CustomerRfm> topCustomerClv = customerClv.stream()
                .sorted(Comparator.comparingDouble(CustomerRfm::monetary).reversed())
                .limit(TOP_N)
                .collect(Collectors.toList());

        List<ShiftPerformance> shiftPerformance = new ArrayList<>();
        BigDecimal cashVarianceTotal = BigDecimal.ZERO;
        int cashVarianceCount = 0;
        List<Shift> shifts = shiftRepo.findAll();
        for (Shift shift : shifts) {
            if (shift.getStatus() != ShiftStatus.CLOSED) continue;
            if (shift.getOpenedAt() == null || shift.getClosedAt() == null) continue;
            double hours = java.time.Duration.between(shift.getOpenedAt(), shift.getClosedAt()).toMinutes() / 60.0;
            if (hours <= 0) continue;
            BigDecimal total = safeAmount(shift.getTotalSales());
            double salesPerHour = total.compareTo(BigDecimal.ZERO) > 0 ? total.doubleValue() / hours : 0.0;
            BigDecimal expectedCash = safeAmount(shift.getOpeningCash()).add(safeAmount(shift.getCashTotal()));
            BigDecimal variance = safeAmount(shift.getClosingCash()).subtract(expectedCash);
            cashVarianceTotal = cashVarianceTotal.add(variance);
            cashVarianceCount++;
            shiftPerformance.add(new ShiftPerformance(
                    shift.getCashierUsername() == null ? "Unknown" : shift.getCashierUsername(),
                    round2(hours),
                    toDouble(total),
                    round2(salesPerHour),
                    toDouble(variance)
            ));
        }
        shiftPerformance = shiftPerformance.stream()
                .sorted(Comparator.comparingDouble(ShiftPerformance::totalSales).reversed())
                .limit(TOP_N)
                .collect(Collectors.toList());
        double cashVarianceAvg = cashVarianceCount > 0
                ? cashVarianceTotal.divide(BigDecimal.valueOf(cashVarianceCount), 4, RoundingMode.HALF_UP).doubleValue()
                : 0.0;

        int repeatRefundCustomers = (int) customerRefunds.values().stream().filter(c -> c >= 2).count();
        List<SimpleStat> exceptionStats = List.of(
                new SimpleStat("Voids", String.valueOf(voidCount), formatMoney(voidTotal)),
                new SimpleStat("Refunds", String.valueOf(refundCount), formatMoney(refundTotal)),
                new SimpleStat("High refunds (> $100)", String.valueOf(highRefundCount), "Check refund policy"),
                new SimpleStat("High discounts (>30%)", String.valueOf(suspiciousDiscountCount), "Review price overrides"),
                new SimpleStat("Price overrides", "Not tracked", "Enable audit logs")
        );
        List<SimpleStat> fraudSignals = List.of(
                new SimpleStat("Repeat refunds (customers)", String.valueOf(repeatRefundCustomers), "Refunds in >=2 sales"),
                new SimpleStat("Split payments", String.valueOf(splitPaymentCount), "Mixed or multi-payment"),
                new SimpleStat("Suspicious discounts", String.valueOf(suspiciousDiscountCount), "Discount > 30%")
        );
        List<SimpleStat> complianceStats = List.of(
                new SimpleStat("Tax total", formatMoney(taxTotal), "VAT/GST reporting"),
                new SimpleStat("Invoice integrity", String.valueOf(invoiceIssueCount), "Missing totals/subtotals"),
                new SimpleStat("Audit trails", "Not configured", "Role actions not logged")
        );

        return new DashboardStats(
                dailyLabels,
                dailySales,
                monthlyLabels,
                monthlySales,
                topProductLabels,
                topProductValues,
                topCategoryLabels,
                topCategoryValues,
                dailyLabels,
                paymentCash,
                paymentCard,
                paymentQr,
                revenueShare.labels,
                revenueShare.values,
                currencyShare.labels,
                currencyShare.values,
                topQtyLabels,
                topQtyValues,
                dailyLabels,
                grossRevenueValues,
                grossCostValues,
                grossProfitValues,
                toDouble(grossRevenueTotal),
                toDouble(grossProfitTotal),
                grossMarginPercent,
                lowStockTrendLabels,
                lowStockTrendCounts,
                lowStockCategoryLabels,
                lowStockCategoryValues,
                heatmapDays,
                heatmapHours,
                heatmapValues,
                heatmapMax,
                voidCount,
                toDouble(voidTotal),
                fastMovers,
                slowMovers,
                toDouble(netRevenueTotal),
                avgOrderValue,
                avgItemsPerSale,
                saleCount,
                refundCount,
                toDouble(refundTotal),
                toDouble(discountTotal),
                discountRatePercent,
                toDouble(taxTotal),
                splitPaymentRatePercent,
                cashierPerformance,
                dayOfWeekLabels,
                dayOfWeekRevenue,
                hourLabels,
                hourRevenueValues,
                skuPerformance,
                categoryPerformance,
                toDouble(discountRevenue),
                toDouble(discountProfit),
                discountMarginPercent,
                toDouble(nonDiscountRevenue),
                toDouble(nonDiscountProfit),
                nonDiscountMarginPercent,
                toDouble(deadStockCost),
                outOfStockCount,
                lowStockCount,
                sellThroughPercent,
                reorderRecommendations,
                newCustomerCount,
                returningCustomerCount,
                topCustomerRfm,
                topCustomerClv,
                shiftPerformance,
                toDouble(cashVarianceTotal),
                cashVarianceAvg,
                exceptionStats,
                fraudSignals,
                complianceStats
        );
    }

    /**
     * Executes the buildDailyRange operation.
     *
     * @param daysCount Parameter of type {@code int} used by this operation.
     * @return {@code List<LocalDate>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<LocalDate> buildDailyRange(int daysCount) {
        LocalDate today = LocalDate.now();
        List<LocalDate> days = new ArrayList<>();
        for (int i = daysCount - 1; i >= 0; i--) {
            days.add(today.minusDays(i));
        }
        return days;
    }

    /**
     * Executes the buildMonthlyRange operation.
     *
     * @return {@code List<YearMonth>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<YearMonth> buildMonthlyRange() {
        YearMonth current = YearMonth.now();
        List<YearMonth> months = new ArrayList<>();
        for (int i = MONTHS - 1; i >= 0; i--) {
            months.add(current.minusMonths(i));
        }
        return months;
    }

    /**
     * Executes the topEntries operation.
     *
     * @param map Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @param limit Parameter of type {@code int} used by this operation.
     * @return {@code List<Map.Entry<String, BigDecimal>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<Map.Entry<String, BigDecimal>> topEntries(Map<String, BigDecimal> map, int limit) {
        return map.entrySet().stream()
                .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                .limit(limit)
                .collect(Collectors.toList());
    }

    /**
     * Executes the topEntriesInt operation.
     *
     * @param map Parameter of type {@code Map<String, Integer>} used by this operation.
     * @param limit Parameter of type {@code int} used by this operation.
     * @return {@code List<Map.Entry<String, Integer>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<Map.Entry<String, Integer>> topEntriesInt(Map<String, Integer> map, int limit) {
        return map.entrySet().stream()
                .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                .limit(limit)
                .collect(Collectors.toList());
    }

    /**
     * Executes the buildRevenueShare operation.
     *
     * @param categoryTotals Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @param limit Parameter of type {@code int} used by this operation.
     * @return {@code RevenueShare} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private RevenueShare buildRevenueShare(Map<String, BigDecimal> categoryTotals, int limit) {
        List<Map.Entry<String, BigDecimal>> sorted = categoryTotals.entrySet().stream()
                .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                .collect(Collectors.toList());

        List<String> labels = new ArrayList<>();
        List<Double> values = new ArrayList<>();
        BigDecimal other = BigDecimal.ZERO;

        for (int i = 0; i < sorted.size(); i++) {
            Map.Entry<String, BigDecimal> entry = sorted.get(i);
            if (i < limit) {
                labels.add(entry.getKey());
                values.add(toDouble(entry.getValue()));
            } else {
                other = other.add(entry.getValue());
            }
        }

        if (other.compareTo(BigDecimal.ZERO) > 0) {
            labels.add("Other");
            values.add(toDouble(other));
        }

        return new RevenueShare(labels, values);
    }

    /**
     * Executes the lineTotal operation.
     *
     * @param item Parameter of type {@code SaleItem} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal lineTotal(SaleItem item) {
        int qty = item.getQty() == null ? 0 : item.getQty();
        int returned = item.getReturnedQty() == null ? 0 : item.getReturnedQty();
        int effectiveQty = Math.max(0, qty - returned);
        if (item.getLineTotal() != null && effectiveQty == qty) return item.getLineTotal();
        if (item.getUnitPrice() != null) {
            return item.getUnitPrice().multiply(BigDecimal.valueOf(effectiveQty));
        }
        return BigDecimal.ZERO;
    }

    /**
     * Executes the unitSize operation.
     *
     * @param item Parameter of type {@code SaleItem} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int unitSize(SaleItem item) {
        if (item == null) return 1;
        Integer size = item.getUnitSize();
        return size == null || size <= 0 ? 1 : size;
    }

    /**
     * Executes the safeAmount operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeAmount(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    /**
     * Executes the toDouble operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code double} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private double toDouble(BigDecimal value) {
        if (value == null) return 0.0;
        return value.setScale(2, RoundingMode.HALF_UP).doubleValue();
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
     * Executes the formatMoney operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String formatMoney(BigDecimal value) {
        return "$" + BigDecimal.ZERO.add(safeAmount(value)).setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Executes the rfmScore operation.
     *
     * @param recencyDays Parameter of type {@code int} used by this operation.
     * @param frequency Parameter of type {@code int} used by this operation.
     * @param monetary Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int rfmScore(int recencyDays, int frequency, BigDecimal monetary) {
        int recencyScore;
        if (recencyDays <= 30) recencyScore = 5;
        else if (recencyDays <= 60) recencyScore = 4;
        else if (recencyDays <= 90) recencyScore = 3;
        else if (recencyDays <= 180) recencyScore = 2;
        else recencyScore = 1;

        int frequencyScore;
        if (frequency >= 10) frequencyScore = 5;
        else if (frequency >= 6) frequencyScore = 4;
        else if (frequency >= 3) frequencyScore = 3;
        else if (frequency >= 2) frequencyScore = 2;
        else frequencyScore = 1;

        BigDecimal spend = monetary == null ? BigDecimal.ZERO : monetary;
        int monetaryScore;
        if (spend.compareTo(new BigDecimal("1000")) >= 0) monetaryScore = 5;
        else if (spend.compareTo(new BigDecimal("500")) >= 0) monetaryScore = 4;
        else if (spend.compareTo(new BigDecimal("200")) >= 0) monetaryScore = 3;
        else if (spend.compareTo(new BigDecimal("100")) >= 0) monetaryScore = 2;
        else monetaryScore = 1;

        return recencyScore + frequencyScore + monetaryScore;
    }

    private static class RevenueShare {
        private final List<String> labels;
        private final List<Double> values;

        /**
         * Executes the RevenueShare operation.
         * <p>Return value: A fully initialized RevenueShare instance.</p>
         *
         * @param labels Parameter of type {@code List<String>} used by this operation.
         * @param values Parameter of type {@code List<Double>} used by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private RevenueShare(List<String> labels, List<Double> values) {
            this.labels = labels;
            this.values = values;
        }
    }

    private static class RevenueCost {
        private BigDecimal revenue = BigDecimal.ZERO;
        private BigDecimal cost = BigDecimal.ZERO;
        private int qty = 0;
    }

    private static class CashierAccumulator {
        private BigDecimal revenue = BigDecimal.ZERO;
        private int transactions = 0;
        private int items = 0;
        private Set<String> hours = new java.util.HashSet<>();
    }

    private static class CustomerAccumulator {
        private final String name;
        private int frequency = 0;
        private BigDecimal monetary = BigDecimal.ZERO;
        private LocalDateTime lastPurchase;

        /**
         * Executes the CustomerAccumulator operation.
         * <p>Return value: A fully initialized CustomerAccumulator instance.</p>
         *
         * @param name Parameter of type {@code String} used by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private CustomerAccumulator(String name) {
            this.name = name;
        }
    }
}
