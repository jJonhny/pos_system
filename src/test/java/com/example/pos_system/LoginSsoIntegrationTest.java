package com.example.pos_system;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest(properties = {
        "app.auth.sso.enabled=true",
        "app.auth.sso.demo.enabled=true",
        "app.auth.sso.registration-id=corp"
})
@AutoConfigureMockMvc
@ActiveProfiles("test")
class LoginSsoIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    /**
     * Executes the loginPageShowsSsoActionWhenConfigured operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the loginPageShowsSsoActionWhenConfigured operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the loginPageShowsSsoActionWhenConfigured operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void loginPageShowsSsoActionWhenConfigured() throws Exception {
        mockMvc.perform(get("/login"))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("/login/sso")));
    }

    /**
     * Executes the ssoRouteRedirectsToSpringAuthorizationEndpoint operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the ssoRouteRedirectsToSpringAuthorizationEndpoint operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the ssoRouteRedirectsToSpringAuthorizationEndpoint operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void ssoRouteRedirectsToSpringAuthorizationEndpoint() throws Exception {
        mockMvc.perform(get("/login/sso"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/oauth2/authorization/corp"));
    }
}
