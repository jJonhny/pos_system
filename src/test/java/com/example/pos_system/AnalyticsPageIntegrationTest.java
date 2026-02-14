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
                .andExpect(content().string(org.hamcrest.Matchers.containsString("cashierEfficiencyChart")));
    }
}
