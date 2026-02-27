package com.example.pos_system;

import com.example.pos_system.entity.AppUser;
import com.example.pos_system.repository.AppUserRepo;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDateTime;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrlPattern;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
class LoginActionsIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private AppUserRepo appUserRepo;

    /**
     * Executes the loginPageIncludesBackendActionLinks operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the loginPageIncludesBackendActionLinks operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the loginPageIncludesBackendActionLinks operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void loginPageIncludesBackendActionLinks() throws Exception {
        mockMvc.perform(get("/login"))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("/login/forgot-password")))
                .andExpect(content().string(containsString("Username/password sign-in only for this environment.")))
                .andExpect(content().string(containsString("/support/contact")))
                .andExpect(content().string(containsString("/legal/privacy")))
                .andExpect(content().string(containsString("/legal/terms")));
    }

    /**
     * Executes the forgotPasswordPageRendersForAnonymousUser operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the forgotPasswordPageRendersForAnonymousUser operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the forgotPasswordPageRendersForAnonymousUser operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void forgotPasswordPageRendersForAnonymousUser() throws Exception {
        mockMvc.perform(get("/login/forgot-password"))
                .andExpect(status().isOk())
                .andExpect(content().string(containsString("Forgot password?")));
    }

    /**
     * Executes the forgotPasswordRequestRedirectsBackToLoginWithConfirmation operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the forgotPasswordRequestRedirectsBackToLoginWithConfirmation operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the forgotPasswordRequestRedirectsBackToLoginWithConfirmation operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void forgotPasswordRequestRedirectsBackToLoginWithConfirmation() throws Exception {
        mockMvc.perform(post("/login/forgot-password")
                        .param("username", "cashier")
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/login?resetRequested=1"));
    }

    /**
     * Executes the ssoEndpointRedirectsToLoginWithMessage operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the ssoEndpointRedirectsToLoginWithMessage operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the ssoEndpointRedirectsToLoginWithMessage operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void ssoEndpointRedirectsToLoginWithMessage() throws Exception {
        mockMvc.perform(get("/login/sso"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/login?ssoUnavailable=1"));
    }

    /**
     * Executes the supportAndLegalPagesArePublic operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the supportAndLegalPagesArePublic operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the supportAndLegalPagesArePublic operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the repeatedBadCredentialsTemporarilyLockUser operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the repeatedBadCredentialsTemporarilyLockUser operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the repeatedBadCredentialsTemporarilyLockUser operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void repeatedBadCredentialsTemporarilyLockUser() throws Exception {
        for (int i = 0; i < 4; i++) {
            mockMvc.perform(post("/login")
                            .param("username", "cashier")
                            .param("password", "bad-password")
                            .with(SecurityMockMvcRequestPostProcessors.csrf()))
                    .andExpect(status().is3xxRedirection())
                    .andExpect(redirectedUrl("/login?error=1&reason=bad-credentials"));
        }

        mockMvc.perform(post("/login")
                        .param("username", "cashier")
                        .param("password", "bad-password")
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrlPattern("/login?error=1&reason=locked*"));

        AppUser lockedUser = appUserRepo.findByUsernameIgnoreCase("cashier").orElseThrow();
        assertThat(lockedUser.getLockedUntil()).isNotNull();
        assertThat(lockedUser.getLockedUntil()).isAfter(LocalDateTime.now().minusSeconds(1));
        assertThat(lockedUser.getLastFailedLoginAt()).isNotNull();
    }

    /**
     * Executes the successfulLoginClearsFailureTracking operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the successfulLoginClearsFailureTracking operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the successfulLoginClearsFailureTracking operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void successfulLoginClearsFailureTracking() throws Exception {
        AppUser admin = appUserRepo.findByUsernameIgnoreCase("admin").orElseThrow();
        admin.setFailedLoginAttempts(3);
        admin.setLastFailedLoginAt(LocalDateTime.now().minusMinutes(1));
        admin.setLockedUntil(null);
        appUserRepo.save(admin);

        mockMvc.perform(post("/login")
                        .param("username", "admin")
                        .param("password", "admin123")
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/"));

        AppUser refreshed = appUserRepo.findByUsernameIgnoreCase("admin").orElseThrow();
        assertThat(refreshed.getFailedLoginAttempts()).isZero();
        assertThat(refreshed.getLastFailedLoginAt()).isNull();
        assertThat(refreshed.getLockedUntil()).isNull();
    }
}
