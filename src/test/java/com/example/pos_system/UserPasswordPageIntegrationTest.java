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
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
class UserPasswordPageIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void passwordPageRendersForAuthenticatedUser() throws Exception {
        mockMvc.perform(get("/users/password")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER")))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("Reset your password")));
    }
}
