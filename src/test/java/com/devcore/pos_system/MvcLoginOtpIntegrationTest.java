package com.devcore.pos_system;

import com.devcore.pos_system.entity.AppUser;
import com.devcore.pos_system.entity.UserRole;
import com.devcore.pos_system.repository.AppUserRepo;
import com.devcore.pos_system.service.SpeakeasyTotpService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.mock.web.MockHttpSession;

import java.time.LocalDateTime;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrlPattern;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
@TestPropertySource(properties = {
        "app.security.login.max-failed-attempts=3",
        "app.security.login.lock-duration-minutes=15"
})
class MvcLoginOtpIntegrationTest {
    private static final String SESSION_OTP_CHALLENGE = "LOGIN_OTP_CHALLENGE";
    private static final String PASSWORD = "Admin123!";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private AppUserRepo appUserRepo;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    private StubSpeakeasyTotpService speakeasyTotpService;

    @BeforeEach
    void setUp() {
        speakeasyTotpService.setVerifyResult(false);
        speakeasyTotpService.resetCounters();
    }

    @Test
    void passwordStepShouldStartOtpChallenge() throws Exception {
        TestUser user = createOtpEnabledUser();

        MockHttpSession session = startOtpChallenge(user);

        mockMvc.perform(get("/login").session(session))
                .andExpect(status().isOk())
                .andExpect(model().attribute("otpPending", true))
                .andExpect(model().attribute("otpFirstTimeSetup", false));
    }

    @Test
    void verifyOtpShouldRejectInvalidCodeAndKeepChallenge() throws Exception {
        TestUser user = createOtpEnabledUser();
        MockHttpSession session = startOtpChallenge(user);
        speakeasyTotpService.setVerifyResult(false);

        MvcResult otpResult = mockMvc.perform(post("/login/otp/verify")
                        .with(csrf())
                        .session(session)
                        .param("otpCode", "000000"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/login?otp=1&otpError=invalid"))
                .andReturn();

        MockHttpSession updatedSession = (MockHttpSession) otpResult.getRequest().getSession(false);
        assertNotNull(updatedSession);
        assertNotNull(updatedSession.getAttribute(SESSION_OTP_CHALLENGE));
    }

    @Test
    void verifyOtpShouldLockAccountAfterRepeatedFailures() throws Exception {
        TestUser user = createOtpEnabledUser();
        MockHttpSession session = startOtpChallenge(user);
        speakeasyTotpService.setVerifyResult(false);

        mockMvc.perform(post("/login/otp/verify")
                        .with(csrf())
                        .session(session)
                        .param("otpCode", "000001"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/login?otp=1&otpError=invalid"));

        mockMvc.perform(post("/login/otp/verify")
                        .with(csrf())
                        .session(session)
                        .param("otpCode", "000002"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/login?otp=1&otpError=invalid"));

        MvcResult lockResult = mockMvc.perform(post("/login/otp/verify")
                        .with(csrf())
                        .session(session)
                        .param("otpCode", "000003"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrlPattern("/login?error=1&reason=locked&lockedMinutes=*"))
                .andReturn();

        MockHttpSession lockedSession = (MockHttpSession) lockResult.getRequest().getSession(false);
        assertNotNull(lockedSession);
        assertNull(lockedSession.getAttribute(SESSION_OTP_CHALLENGE));

        AppUser lockedUser = appUserRepo.findById(user.id()).orElseThrow();
        assertNotNull(lockedUser.getLockedUntil());
        assertTrue(lockedUser.getLockedUntil().isAfter(LocalDateTime.now()));
    }

    @Test
    void verifyOtpShouldCreateAuthenticatedSessionOnSuccess() throws Exception {
        TestUser user = createOtpEnabledUser();
        MockHttpSession session = startOtpChallenge(user);
        speakeasyTotpService.setVerifyResult(true);

        MvcResult successResult = mockMvc.perform(post("/login/otp/verify")
                        .with(csrf())
                        .session(session)
                        .param("otpCode", "123456"))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/"))
                .andReturn();

        MockHttpSession authenticatedSession = (MockHttpSession) successResult.getRequest().getSession(false);
        assertNotNull(authenticatedSession);
        assertNull(authenticatedSession.getAttribute(SESSION_OTP_CHALLENGE));
        assertNotNull(authenticatedSession.getAttribute(
                HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY));
    }

    @Test
    void passwordStepShouldReuseExistingSecretForPendingFirstTimeSetup() throws Exception {
        String existingSecret = "AAAAAAAAAAAAAAAA";
        TestUser user = createPendingTotpSetupUser(existingSecret);

        mockMvc.perform(post("/login/password")
                        .with(csrf())
                        .param("username", user.identifier())
                        .param("password", PASSWORD))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/login?otp=1"));

        AppUser updated = appUserRepo.findById(user.id()).orElseThrow();
        assertEquals(existingSecret, updated.getTotpSecret());
        assertEquals(0, speakeasyTotpService.generateSetupCalls());
        assertEquals(1, speakeasyTotpService.buildFromSecretCalls());
    }

    private MockHttpSession startOtpChallenge(TestUser user) throws Exception {
        MvcResult result = mockMvc.perform(post("/login/password")
                        .with(csrf())
                        .param("username", user.identifier())
                        .param("password", PASSWORD))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/login?otp=1"))
                .andReturn();

        MockHttpSession session = (MockHttpSession) result.getRequest().getSession(false);
        assertNotNull(session);
        assertNotNull(session.getAttribute(SESSION_OTP_CHALLENGE));
        return session;
    }

    private TestUser createOtpEnabledUser() {
        String suffix = UUID.randomUUID().toString().substring(0, 8);
        String username = "mvc.otp." + suffix;
        String email = username + "@example.com";

        AppUser user = new AppUser();
        user.setUsername(username);
        user.setEmail(email);
        user.setPassword(passwordEncoder.encode(PASSWORD));
        user.setRole(UserRole.CASHIER);
        user.setActive(true);
        user.setTotpEnabled(true);
        user.setTotpSecret("JBSWY3DPEHPK3PXP");

        AppUser saved = appUserRepo.save(user);
        return new TestUser(saved.getId(), username);
    }

    private TestUser createPendingTotpSetupUser(String secret) {
        String suffix = UUID.randomUUID().toString().substring(0, 8);
        String username = "mvc.pending." + suffix;
        String email = username + "@example.com";

        AppUser user = new AppUser();
        user.setUsername(username);
        user.setEmail(email);
        user.setPassword(passwordEncoder.encode(PASSWORD));
        user.setRole(UserRole.CASHIER);
        user.setActive(true);
        user.setTotpEnabled(false);
        user.setTotpSecret(secret);

        AppUser saved = appUserRepo.save(user);
        return new TestUser(saved.getId(), username);
    }

    private record TestUser(Long id, String identifier) {
    }

    @TestConfiguration
    static class TestOverrides {
        @Bean
        @Primary
        StubSpeakeasyTotpService stubSpeakeasyTotpService() {
            return new StubSpeakeasyTotpService();
        }
    }

    static class StubSpeakeasyTotpService extends SpeakeasyTotpService {
        private volatile boolean verifyResult;
        private volatile int generateSetupCalls;
        private volatile int buildFromSecretCalls;

        void setVerifyResult(boolean verifyResult) {
            this.verifyResult = verifyResult;
        }

        void resetCounters() {
            generateSetupCalls = 0;
            buildFromSecretCalls = 0;
        }

        int generateSetupCalls() {
            return generateSetupCalls;
        }

        int buildFromSecretCalls() {
            return buildFromSecretCalls;
        }

        @Override
        public SetupPayload generateSetup(String label) {
            generateSetupCalls++;
            String normalizedLabel = (label == null || label.isBlank()) ? "user@example.com" : label.trim();
            return new SetupPayload(
                    "JBSWY3DPEHPK3PXP",
                    "otpauth://totp/DevCore%20POS:" + normalizedLabel + "?secret=JBSWY3DPEHPK3PXP&issuer=DevCore%20POS",
                    "data:image/png;base64,stub"
            );
        }

        @Override
        public SetupPayload buildSetupFromSecret(String base32Secret, String label) {
            buildFromSecretCalls++;
            String normalizedLabel = (label == null || label.isBlank()) ? "user@example.com" : label.trim();
            String normalizedSecret = (base32Secret == null || base32Secret.isBlank()) ? "JBSWY3DPEHPK3PXP" : base32Secret.trim();
            return new SetupPayload(
                    normalizedSecret,
                    "otpauth://totp/DevCore%20POS:" + normalizedLabel + "?secret=" + normalizedSecret + "&issuer=DevCore%20POS",
                    "data:image/png;base64,stub"
            );
        }

        @Override
        public boolean verifyCode(String base32Secret, String code) {
            return verifyResult;
        }
    }
}
