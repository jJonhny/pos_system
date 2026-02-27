package com.example.pos_system;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.flash;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
class UserPasswordPageIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    /**
     * Executes the passwordPageRendersForAuthenticatedUser operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the passwordPageRendersForAuthenticatedUser operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the passwordPageRendersForAuthenticatedUser operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void passwordPageRendersForAuthenticatedUser() throws Exception {
        mockMvc.perform(get("/users/password")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER")))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("Reset your password")));
    }

    /**
     * Executes the captchaEndpointStoresOneTimeCode operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the captchaEndpointStoresOneTimeCode operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the captchaEndpointStoresOneTimeCode operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void captchaEndpointStoresOneTimeCode() throws Exception {
        MvcResult result = mockMvc.perform(post("/users/password/captcha")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/users/password"))
                .andExpect(flash().attributeExists("info"))
                .andReturn();

        MockHttpSession session = (MockHttpSession) result.getRequest().getSession(false);
        org.assertj.core.api.Assertions.assertThat(session).isNotNull();
        org.assertj.core.api.Assertions.assertThat(session.getAttribute("users.password.captcha.code")).isNotNull();
    }

    /**
     * Executes the passwordUpdateRequiresCaptchaAndConfirmation operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the passwordUpdateRequiresCaptchaAndConfirmation operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the passwordUpdateRequiresCaptchaAndConfirmation operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void passwordUpdateRequiresCaptchaAndConfirmation() throws Exception {
        MvcResult captchaResult = mockMvc.perform(post("/users/password/captcha")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().is3xxRedirection())
                .andReturn();

        MockHttpSession session = (MockHttpSession) captchaResult.getRequest().getSession(false);
        String code = session == null ? null : (String) session.getAttribute("users.password.captcha.code");
        org.assertj.core.api.Assertions.assertThat(session).isNotNull();
        org.assertj.core.api.Assertions.assertThat(code).isNotBlank();

        mockMvc.perform(post("/users/password")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf())
                        .session(session)
                        .param("password", "StrongPass!123")
                        .param("confirmPassword", "StrongPass!123")
                        .param("captcha", code))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/"))
                .andExpect(flash().attributeExists("success"));
    }
}
