package com.example.pos_system.controller;

import com.example.pos_system.dto.DashboardStats;
import com.example.pos_system.service.DashboardService;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class HomeController {
    private final DashboardService dashboardService;

    public HomeController(DashboardService dashboardService) {
        this.dashboardService = dashboardService;
    }

    @GetMapping("/")
    public String home() {
        return "forward:/pos";
    }

    @GetMapping("/analytics")
    public String analytics(Model model) {
        populateDashboard(model);
        return "analytics";
    }

    private void populateDashboard(Model model) {
        DashboardStats stats = dashboardService.buildStats();
        model.addAttribute("dailyLabels", stats.dailyLabels());
        model.addAttribute("dailySales", stats.dailySales());
        model.addAttribute("monthlyLabels", stats.monthlyLabels());
        model.addAttribute("monthlySales", stats.monthlySales());
        model.addAttribute("topProductLabels", stats.topProductLabels());
        model.addAttribute("topProductValues", stats.topProductValues());
        model.addAttribute("topCategoryLabels", stats.topCategoryLabels());
        model.addAttribute("topCategoryValues", stats.topCategoryValues());
        model.addAttribute("paymentLabels", stats.paymentLabels());
        model.addAttribute("paymentCash", stats.paymentCash());
        model.addAttribute("paymentCard", stats.paymentCard());
        model.addAttribute("paymentQr", stats.paymentQr());
        model.addAttribute("revenueShareLabels", stats.revenueShareLabels());
        model.addAttribute("revenueShareValues", stats.revenueShareValues());
        model.addAttribute("currencyShareLabels", stats.currencyShareLabels());
        model.addAttribute("currencyShareValues", stats.currencyShareValues());
        model.addAttribute("topQtyLabels", stats.topQtyLabels());
        model.addAttribute("topQtyValues", stats.topQtyValues());
        model.addAttribute("grossProfitLabels", stats.grossProfitLabels());
        model.addAttribute("grossRevenueValues", stats.grossRevenueValues());
        model.addAttribute("grossCostValues", stats.grossCostValues());
        model.addAttribute("grossProfitValues", stats.grossProfitValues());
        model.addAttribute("grossRevenueTotal", stats.grossRevenueTotal());
        model.addAttribute("grossProfitTotal", stats.grossProfitTotal());
        model.addAttribute("grossMarginPercent", stats.grossMarginPercent());
        model.addAttribute("heatmapDays", stats.heatmapDays());
        model.addAttribute("heatmapHours", stats.heatmapHours());
        model.addAttribute("heatmapValues", stats.heatmapValues());
        model.addAttribute("heatmapMax", stats.heatmapMax());
        model.addAttribute("voidCount", stats.voidCount());
        model.addAttribute("voidTotal", stats.voidTotal());
        model.addAttribute("fastMovers", stats.fastMovers());
        model.addAttribute("slowMovers", stats.slowMovers());
        model.addAttribute("netRevenueTotal", stats.netRevenueTotal());
        model.addAttribute("avgOrderValue", stats.avgOrderValue());
        model.addAttribute("avgItemsPerSale", stats.avgItemsPerSale());
        model.addAttribute("saleCount", stats.saleCount());
        model.addAttribute("refundCount", stats.refundCount());
        model.addAttribute("refundTotal", stats.refundTotal());
        model.addAttribute("discountTotal", stats.discountTotal());
        model.addAttribute("discountRatePercent", stats.discountRatePercent());
        model.addAttribute("taxTotal", stats.taxTotal());
        model.addAttribute("splitPaymentRatePercent", stats.splitPaymentRatePercent());
        model.addAttribute("cashierPerformance", stats.cashierPerformance());
        model.addAttribute("dayOfWeekLabels", stats.dayOfWeekLabels());
        model.addAttribute("dayOfWeekRevenue", stats.dayOfWeekRevenue());
        model.addAttribute("hourLabels", stats.hourLabels());
        model.addAttribute("hourRevenue", stats.hourRevenue());
        model.addAttribute("skuPerformance", stats.skuPerformance());
        model.addAttribute("categoryPerformance", stats.categoryPerformance());
        model.addAttribute("discountRevenue", stats.discountRevenue());
        model.addAttribute("discountProfit", stats.discountProfit());
        model.addAttribute("discountMarginPercent", stats.discountMarginPercent());
        model.addAttribute("nonDiscountRevenue", stats.nonDiscountRevenue());
        model.addAttribute("nonDiscountProfit", stats.nonDiscountProfit());
        model.addAttribute("nonDiscountMarginPercent", stats.nonDiscountMarginPercent());
        model.addAttribute("deadStockCost", stats.deadStockCost());
        model.addAttribute("outOfStockCount", stats.outOfStockCount());
        model.addAttribute("lowStockCount", stats.lowStockCount());
        model.addAttribute("sellThroughPercent", stats.sellThroughPercent());
        model.addAttribute("reorderRecommendations", stats.reorderRecommendations());
        model.addAttribute("newCustomerCount", stats.newCustomerCount());
        model.addAttribute("returningCustomerCount", stats.returningCustomerCount());
        model.addAttribute("topCustomerRfm", stats.topCustomerRfm());
        model.addAttribute("topCustomerClv", stats.topCustomerClv());
        model.addAttribute("shiftPerformance", stats.shiftPerformance());
        model.addAttribute("cashVarianceTotal", stats.cashVarianceTotal());
        model.addAttribute("cashVarianceAvg", stats.cashVarianceAvg());
        model.addAttribute("exceptionStats", stats.exceptionStats());
        model.addAttribute("fraudSignals", stats.fraudSignals());
        model.addAttribute("complianceStats", stats.complianceStats());
    }
}
