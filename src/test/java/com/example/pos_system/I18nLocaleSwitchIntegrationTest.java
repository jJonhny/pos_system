package com.example.pos_system;

import com.example.pos_system.repository.AppUserRepo;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.cookie;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
class I18nLocaleSwitchIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private AppUserRepo appUserRepo;

    @Test
    void langQueryParamSwitchesLocaleToZhCnSetsCookieAndPersistsUserPreference() throws Exception {
        mockMvc.perform(get("/pos")
                        .param("lang", "zh-CN")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER")))
                .andExpect(status().isOk())
                .andExpect(cookie().value("POS_LANG", "zh-CN"))
                .andExpect(content().string(containsString("收银台")));

        String preference = appUserRepo.findByUsername("cashier")
                .map(user -> user.getLanguagePreference())
                .orElse(null);
        assertThat(preference).isEqualToIgnoringCase("zh-CN");
    }
}
