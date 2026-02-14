package com.example.pos_system;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
class AnalyticsPageIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void analyticsPageRendersAdditionalCharts() throws Exception {
        mockMvc.perform(get("/analytics")
                        .with(SecurityMockMvcRequestPostProcessors.user("manager").roles("MANAGER")))
                .andExpect(status().isOk())
                .andExpect(content().string(org.hamcrest.Matchers.containsString("revenueBridgeChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("customerMixChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("marginCompareChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("shiftVarianceChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("cashierEfficiencyChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("cumulativeRevenueChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("salesHealthChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("leakageBreakdownChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("verticalBarChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("horizontalBarChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("groupedBarChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("stackedBarChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("stacked100BarChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("lollipopChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("dotPlotChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("radarComparisonChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("polarComparisonChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("trendLineChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("trendMultiLineChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("trendStepLineChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("trendAreaChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("trendStackedAreaChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("trendStreamgraphChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("trendTimelineChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("trendCandlestickChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("trendOhlcChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("advancedChartGallery")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("Part-to-Whole Charts")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("Distribution Charts")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("Advanced / Specialized Charts")));
    }
}
