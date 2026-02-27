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

    /**
     * Executes the langQueryParamSwitchesLocaleToZhCnSetsCookieAndPersistsUserPreference operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the langQueryParamSwitchesLocaleToZhCnSetsCookieAndPersistsUserPreference operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the langQueryParamSwitchesLocaleToZhCnSetsCookieAndPersistsUserPreference operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the langQueryParamCanSwitchBackFromZhCnToEnglish operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the langQueryParamCanSwitchBackFromZhCnToEnglish operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the langQueryParamCanSwitchBackFromZhCnToEnglish operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void langQueryParamCanSwitchBackFromZhCnToEnglish() throws Exception {
        mockMvc.perform(get("/pos")
                        .param("lang", "zh-CN")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER")))
                .andExpect(status().isOk())
                .andExpect(cookie().value("POS_LANG", "zh-CN"))
                .andExpect(content().string(containsString("收银台")));

        mockMvc.perform(get("/pos")
                        .param("lang", "en")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER")))
                .andExpect(status().isOk())
                .andExpect(cookie().value("POS_LANG", "en"))
                .andExpect(content().string(containsString("Point of Sale")));

        String preference = appUserRepo.findByUsername("cashier")
                .map(user -> user.getLanguagePreference())
                .orElse(null);
        assertThat(preference).isEqualToIgnoringCase("en");
    }
}
