package com.example.pos_system;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
class LoginActionsIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void loginPageIncludesBackendActionLinks() throws Exception {
        mockMvc.perform(get("/login"))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("/login/forgot-password")))
                .andExpect(content().string(containsString("/login/sso")))
                .andExpect(content().string(containsString("/support/contact")))
                .andExpect(content().string(containsString("/legal/privacy")))
                .andExpect(content().string(containsString("/legal/terms")));
    }

    @Test
    void forgotPasswordPageRendersForAnonymousUser() throws Exception {
        mockMvc.perform(get("/login/forgot-password"))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("Forgot your password?")));
    }

    @Test
    void forgotPasswordRequestRedirectsBackToLoginWithConfirmation() throws Exception {
        mockMvc.perform(post("/login/forgot-password")
                        .param("username", "cashier")
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/login?resetRequested=1"));
    }

    @Test
    void ssoEndpointRedirectsToLoginWithMessage() throws Exception {
        mockMvc.perform(get("/login/sso"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/login?ssoUnavailable=1"));
    }

    @Test
    void supportAndLegalPagesArePublic() throws Exception {
        mockMvc.perform(get("/support/contact"))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("Contact IT Support")));

        mockMvc.perform(get("/legal/privacy"))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("Privacy Policy")));

        mockMvc.perform(get("/legal/terms"))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("Terms of Service")));
    }
}
