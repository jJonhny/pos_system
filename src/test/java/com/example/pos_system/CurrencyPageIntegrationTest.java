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
class CurrencyPageIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void currenciesPageRendersAnalyticsWidgets() throws Exception {
        mockMvc.perform(get("/currencies")
                        .with(SecurityMockMvcRequestPostProcessors.user("admin").roles("ADMIN")))
                .andExpect(status().isOk())
                .andExpect(content().string(org.hamcrest.Matchers.containsString("Currency converter")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("rateComparisonChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("volatilityChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("freshnessChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("trendCompareChart")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("converterAmount")))
                .andExpect(content().string(org.hamcrest.Matchers.containsString("currencySearch")));
    }
}
