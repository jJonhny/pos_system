package com.example.pos_system;

import com.example.pos_system.service.SpeakeasyTotpService;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
class AuthApiIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @MockitoBean
    private SpeakeasyTotpService speakeasyTotpService;

    private final ObjectMapper mapper = new ObjectMapper().findAndRegisterModules();

    /**
     * Executes the registerThenLoginReturnsOtpChallengeAndSetupPayload operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the registerThenLoginReturnsOtpChallengeAndSetupPayload operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the registerThenLoginReturnsOtpChallengeAndSetupPayload operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void registerThenLoginReturnsOtpChallengeAndSetupPayload() throws Exception {
        when(speakeasyTotpService.generateSetup(anyString()))
                .thenReturn(new SpeakeasyTotpService.SetupPayload(
                        "JBSWY3DPEHPK3PXP",
                        "otpauth://totp/DevCore:test%40example.com?secret=JBSWY3DPEHPK3PXP&issuer=DevCore",
                        "data:image/png;base64,TESTQR"
                ));

        String email = "api-user-1@example.com";
        String password = "StrongPass123";

        mockMvc.perform(post("/api/v1/auth/register")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "email":"%s",
                                  "password":"%s",
                                  "role":"CASHIER"
                                }
                                """.formatted(email, password)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.email").value(email))
                .andExpect(jsonPath("$.role").value("CASHIER"));

        mockMvc.perform(post("/api/v1/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "email":"%s",
                                  "password":"%s"
                                }
                                """.formatted(email, password)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.status").value("OTP_REQUIRED"))
                .andExpect(jsonPath("$.firstTimeSetup").value(true))
                .andExpect(jsonPath("$.challengeToken").isNotEmpty())
                .andExpect(jsonPath("$.qrDataUrl").value("data:image/png;base64,TESTQR"));
    }

    /**
     * Executes the verifyOtpReturnsJwtAfterSuccessfulChallenge operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the verifyOtpReturnsJwtAfterSuccessfulChallenge operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the verifyOtpReturnsJwtAfterSuccessfulChallenge operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void verifyOtpReturnsJwtAfterSuccessfulChallenge() throws Exception {
        when(speakeasyTotpService.generateSetup(anyString()))
                .thenReturn(new SpeakeasyTotpService.SetupPayload(
                        "JBSWY3DPEHPK3PXP",
                        "otpauth://totp/DevCore:test2%40example.com?secret=JBSWY3DPEHPK3PXP&issuer=DevCore",
                        "data:image/png;base64,TESTQR2"
                ));
        when(speakeasyTotpService.verifyCode(anyString(), anyString())).thenReturn(true);

        String email = "api-user-2@example.com";
        String password = "StrongPass123";

        mockMvc.perform(post("/api/v1/auth/register")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "email":"%s",
                                  "password":"%s",
                                  "role":"BRANCH_MANAGER"
                                }
                                """.formatted(email, password)))
                .andExpect(status().isCreated());

        MvcResult loginResult = mockMvc.perform(post("/api/v1/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "email":"%s",
                                  "password":"%s"
                                }
                                """.formatted(email, password)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.challengeToken").isNotEmpty())
                .andReturn();

        JsonNode loginPayload = mapper.readTree(loginResult.getResponse().getContentAsString());
        String challengeToken = loginPayload.path("challengeToken").asText();
        assertThat(challengeToken).isNotBlank();

        mockMvc.perform(post("/api/v1/auth/verify-otp")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "challengeToken":"%s",
                                  "otpCode":"123456"
                                }
                                """.formatted(challengeToken)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.status").value("SUCCESS"))
                .andExpect(jsonPath("$.accessToken").isNotEmpty())
                .andExpect(jsonPath("$.role").value("BRANCH_MANAGER"))
                .andExpect(jsonPath("$.expiresInSeconds").isNumber());
    }
}
