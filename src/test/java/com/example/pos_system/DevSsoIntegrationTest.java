package com.example.pos_system;

import com.jayway.jsonpath.JsonPath;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.net.URI;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest(properties = {
        "app.auth.sso.enabled=true",
        "app.auth.sso.demo.enabled=true",
        "app.auth.sso.demo.client-id=demo-client",
        "app.auth.sso.demo.client-secret=demo-secret",
        "app.auth.sso.demo.user-sub=demo-subject",
        "app.auth.sso.demo.user-email=demo.user@example.com",
        "app.auth.sso.demo.user-username=demo.user",
        "app.auth.sso.demo.user-name=Demo User"
})
@AutoConfigureMockMvc
@ActiveProfiles("test")
class DevSsoIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    /**
     * Executes the demoSsoAuthorizationCodeFlowReturnsConfiguredUserInfo operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the demoSsoAuthorizationCodeFlowReturnsConfiguredUserInfo operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the demoSsoAuthorizationCodeFlowReturnsConfiguredUserInfo operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void demoSsoAuthorizationCodeFlowReturnsConfiguredUserInfo() throws Exception {
        String redirectUri = "http://localhost/login/oauth2/code/corp";
        String state = "state==123/_-";

        MvcResult authorizeResult = mockMvc.perform(get("/dev-sso/authorize")
                        .param("response_type", "code")
                        .param("client_id", "demo-client")
                        .param("redirect_uri", redirectUri)
                        .param("scope", "profile email")
                        .param("state", state))
                .andExpect(status().is3xxRedirection())
                .andReturn();

        String authorizeLocation = authorizeResult.getResponse().getHeader("Location");
        assertThat(authorizeLocation).isNotBlank();

        Map<String, String> authorizeQuery = readQuery(authorizeLocation);
        assertThat(authorizeQuery).containsEntry("state", state);
        String code = authorizeQuery.get("code");
        assertThat(code).isNotBlank();

        MvcResult tokenResult = mockMvc.perform(post("/dev-sso/token")
                        .contentType(MediaType.APPLICATION_FORM_URLENCODED)
                        .param("grant_type", "authorization_code")
                        .param("code", code)
                        .param("redirect_uri", redirectUri)
                        .param("client_id", "demo-client")
                        .param("client_secret", "demo-secret"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.token_type").value("Bearer"))
                .andExpect(jsonPath("$.access_token").isNotEmpty())
                .andReturn();

        String accessToken = JsonPath.read(tokenResult.getResponse().getContentAsString(), "$.access_token");
        assertThat(accessToken).isNotBlank();

        mockMvc.perform(get("/dev-sso/userinfo")
                        .header("Authorization", "Bearer " + accessToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.sub").value("demo-subject"))
                .andExpect(jsonPath("$.email").value("demo.user@example.com"))
                .andExpect(jsonPath("$.preferred_username").value("demo.user"))
                .andExpect(jsonPath("$.name").value("Demo User"));
    }

    /**
     * Executes the readQuery operation.
     *
     * @param location Parameter of type {@code String} used by this operation.
     * @return {@code Map<String, String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, String> readQuery(String location) {
        URI uri = URI.create(location);
        String query = uri.getRawQuery();
        if (query == null || query.isBlank()) {
            return Map.of();
        }

        return Arrays.stream(query.split("&"))
                .map(this::splitQueryPart)
                .collect(Collectors.toMap(entry -> entry[0], entry -> entry[1], (left, right) -> right));
    }

    /**
     * Executes the splitQueryPart operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String[]} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String[] splitQueryPart(String value) {
        int separator = value.indexOf('=');
        if (separator < 0) {
            return new String[]{decode(value), ""};
        }
        String key = value.substring(0, separator);
        String val = value.substring(separator + 1);
        return new String[]{decode(key), decode(val)};
    }

    /**
     * Executes the decode operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String decode(String value) {
        return URLDecoder.decode(value, StandardCharsets.UTF_8);
    }
}
