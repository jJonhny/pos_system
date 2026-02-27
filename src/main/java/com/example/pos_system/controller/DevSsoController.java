package com.example.pos_system.controller;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.util.UriComponentsBuilder;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@RestController
public class DevSsoController {
    private static final long AUTHORIZATION_CODE_TTL_SECONDS = 180;
    private static final long ACCESS_TOKEN_TTL_SECONDS = 3600;

    private final boolean demoEnabled;
    private final String demoClientId;
    private final String demoClientSecret;
    private final String demoSubject;
    private final String demoEmail;
    private final String demoUsername;
    private final String demoName;

    private final ConcurrentHashMap<String, AuthorizationCodeGrant> authorizationCodes = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, AccessTokenGrant> accessTokens = new ConcurrentHashMap<>();

    /**
     * Executes the DevSsoController operation.
     * <p>Return value: A fully initialized DevSsoController instance.</p>
     *
     * @param demoEnabled Parameter of type {@code boolean} used by this operation.
     * @param demoClientId Parameter of type {@code String} used by this operation.
     * @param demoClientSecret Parameter of type {@code String} used by this operation.
     * @param demoSubject Parameter of type {@code String} used by this operation.
     * @param demoEmail Parameter of type {@code String} used by this operation.
     * @param demoUsername Parameter of type {@code String} used by this operation.
     * @param demoName Parameter of type {@code String} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public DevSsoController(@Value("${app.auth.sso.demo.enabled:true}") boolean demoEnabled,
                            @Value("${app.auth.sso.demo.client-id:demo-client}") String demoClientId,
                            @Value("${app.auth.sso.demo.client-secret:demo-secret}") String demoClientSecret,
                            @Value("${app.auth.sso.demo.user-sub:demo-user-001}") String demoSubject,
                            @Value("${app.auth.sso.demo.user-email:demo.cashier@devcore.local}") String demoEmail,
                            @Value("${app.auth.sso.demo.user-username:demo.cashier}") String demoUsername,
                            @Value("${app.auth.sso.demo.user-name:Demo Cashier}") String demoName) {
        this.demoEnabled = demoEnabled;
        this.demoClientId = normalizeOrDefault(demoClientId, "demo-client");
        this.demoClientSecret = normalizeOrDefault(demoClientSecret, "demo-secret");
        this.demoSubject = normalizeOrDefault(demoSubject, "demo-user-001");
        this.demoEmail = normalizeOrDefault(demoEmail, "demo.cashier@devcore.local");
        this.demoUsername = normalizeOrDefault(demoUsername, "demo.cashier");
        this.demoName = normalizeOrDefault(demoName, "Demo Cashier");
    }

    /**
     * Executes the authorize operation.
     *
     * @param responseType Parameter of type {@code String} used by this operation.
     * @param clientId Parameter of type {@code String} used by this operation.
     * @param redirectUri Parameter of type {@code String} used by this operation.
     * @param scope Parameter of type {@code String} used by this operation.
     * @param state Parameter of type {@code String} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the authorize operation.
     *
     * @param responseType Parameter of type {@code String} used by this operation.
     * @param clientId Parameter of type {@code String} used by this operation.
     * @param redirectUri Parameter of type {@code String} used by this operation.
     * @param scope Parameter of type {@code String} used by this operation.
     * @param state Parameter of type {@code String} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the authorize operation.
     *
     * @param responseType Parameter of type {@code String} used by this operation.
     * @param clientId Parameter of type {@code String} used by this operation.
     * @param redirectUri Parameter of type {@code String} used by this operation.
     * @param scope Parameter of type {@code String} used by this operation.
     * @param state Parameter of type {@code String} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/dev-sso/authorize")
    public ResponseEntity<Void> authorize(@RequestParam(name = "response_type", required = false) String responseType,
                                          @RequestParam(name = "client_id", required = false) String clientId,
                                          @RequestParam(name = "redirect_uri", required = false) String redirectUri,
                                          @RequestParam(name = "scope", required = false) String scope,
                                          @RequestParam(name = "state", required = false) String state) {
        requireDemoEnabled();
        cleanupExpiredGrants();

        String normalizedRedirectUri = normalize(redirectUri);
        if (normalizedRedirectUri == null) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "redirect_uri is required.");
        }
        if (!"code".equalsIgnoreCase(normalize(responseType))) {
            return redirectWithError(normalizedRedirectUri, state, "unsupported_response_type");
        }
        if (!demoClientId.equals(normalize(clientId))) {
            return redirectWithError(normalizedRedirectUri, state, "unauthorized_client");
        }

        String code = randomToken();
        AuthorizationCodeGrant grant = new AuthorizationCodeGrant(
                demoClientId,
                normalizedRedirectUri,
                normalize(scope),
                Map.copyOf(buildUserClaims()),
                Instant.now().plusSeconds(AUTHORIZATION_CODE_TTL_SECONDS)
        );
        authorizationCodes.put(code, grant);

        String redirectLocation = UriComponentsBuilder.fromUriString(normalizedRedirectUri)
                .queryParam("code", code)
                .queryParamIfPresent("state", Optional.ofNullable(normalize(state)))
                .build()
                .toUriString();
        return redirect(redirectLocation);
    }

    /**
     * Executes the token operation.
     *
     * @param form Parameter of type {@code MultiValueMap<String, String>} used by this operation.
     * @param authorizationHeader Parameter of type {@code String} used by this operation.
     * @return {@code ResponseEntity<Map<String, Object>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the token operation.
     *
     * @param form Parameter of type {@code MultiValueMap<String, String>} used by this operation.
     * @param authorizationHeader Parameter of type {@code String} used by this operation.
     * @return {@code ResponseEntity<Map<String, Object>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the token operation.
     *
     * @param form Parameter of type {@code MultiValueMap<String, String>} used by this operation.
     * @param authorizationHeader Parameter of type {@code String} used by this operation.
     * @return {@code ResponseEntity<Map<String, Object>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping(value = "/dev-sso/token", consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Map<String, Object>> token(@RequestParam MultiValueMap<String, String> form,
                                                     @RequestHeader(value = HttpHeaders.AUTHORIZATION, required = false) String authorizationHeader) {
        requireDemoEnabled();
        cleanupExpiredGrants();

        String grantType = normalize(form.getFirst("grant_type"));
        if (!"authorization_code".equals(grantType)) {
            return oauthError(HttpStatus.BAD_REQUEST, "unsupported_grant_type");
        }

        ClientCredentials clientCredentials = extractClientCredentials(form, authorizationHeader);
        if (clientCredentials == null
                || !demoClientId.equals(clientCredentials.clientId())
                || !demoClientSecret.equals(clientCredentials.clientSecret())) {
            return oauthError(HttpStatus.UNAUTHORIZED, "invalid_client");
        }

        String code = normalize(form.getFirst("code"));
        if (code == null) {
            return oauthError(HttpStatus.BAD_REQUEST, "invalid_request");
        }

        AuthorizationCodeGrant grant = authorizationCodes.remove(code);
        if (grant == null || grant.expiresAt().isBefore(Instant.now())) {
            return oauthError(HttpStatus.BAD_REQUEST, "invalid_grant");
        }

        String redirectUri = normalize(form.getFirst("redirect_uri"));
        if (redirectUri == null || !redirectUri.equals(grant.redirectUri())) {
            return oauthError(HttpStatus.BAD_REQUEST, "invalid_grant");
        }

        String accessToken = randomToken();
        accessTokens.put(accessToken, new AccessTokenGrant(
                grant.claims(),
                normalizeOrDefault(grant.scope(), "profile email"),
                Instant.now().plusSeconds(ACCESS_TOKEN_TTL_SECONDS)
        ));

        Map<String, Object> body = new LinkedHashMap<>();
        body.put("access_token", accessToken);
        body.put("token_type", "Bearer");
        body.put("expires_in", ACCESS_TOKEN_TTL_SECONDS);
        body.put("scope", normalizeOrDefault(grant.scope(), "profile email"));

        HttpHeaders headers = new HttpHeaders();
        headers.setCacheControl("no-store");
        headers.setPragma("no-cache");
        return new ResponseEntity<>(body, headers, HttpStatus.OK);
    }

    /**
     * Executes the userInfo operation.
     *
     * @param authorizationHeader Parameter of type {@code String} used by this operation.
     * @return {@code ResponseEntity<Map<String, Object>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the userInfo operation.
     *
     * @param authorizationHeader Parameter of type {@code String} used by this operation.
     * @return {@code ResponseEntity<Map<String, Object>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the userInfo operation.
     *
     * @param authorizationHeader Parameter of type {@code String} used by this operation.
     * @return {@code ResponseEntity<Map<String, Object>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping(value = "/dev-sso/userinfo", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Map<String, Object>> userInfo(@RequestHeader(value = HttpHeaders.AUTHORIZATION, required = false) String authorizationHeader) {
        requireDemoEnabled();
        cleanupExpiredGrants();

        String token = bearerToken(authorizationHeader);
        if (token == null) {
            return oauthError(HttpStatus.UNAUTHORIZED, "invalid_token");
        }

        AccessTokenGrant grant = accessTokens.get(token);
        if (grant == null || grant.expiresAt().isBefore(Instant.now())) {
            accessTokens.remove(token);
            return oauthError(HttpStatus.UNAUTHORIZED, "invalid_token");
        }

        return ResponseEntity.ok(new LinkedHashMap<>(grant.claims()));
    }

    /**
     * Executes the buildUserClaims operation.
     *
     * @return {@code Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, Object> buildUserClaims() {
        Map<String, Object> claims = new LinkedHashMap<>();
        claims.put("sub", demoSubject);
        claims.put("email", demoEmail);
        claims.put("preferred_username", demoUsername);
        claims.put("name", demoName);
        claims.put("email_verified", true);
        return claims;
    }

    /**
     * Executes the extractClientCredentials operation.
     *
     * @param form Parameter of type {@code MultiValueMap<String, String>} used by this operation.
     * @param authorizationHeader Parameter of type {@code String} used by this operation.
     * @return {@code ClientCredentials} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private ClientCredentials extractClientCredentials(MultiValueMap<String, String> form, String authorizationHeader) {
        String fromHeader = normalize(authorizationHeader);
        if (fromHeader != null && fromHeader.startsWith("Basic ")) {
            String encoded = fromHeader.substring(6).trim();
            try {
                String decoded = new String(Base64.getDecoder().decode(encoded), StandardCharsets.UTF_8);
                int separator = decoded.indexOf(':');
                if (separator > -1) {
                    String clientId = normalize(decoded.substring(0, separator));
                    String clientSecret = normalize(decoded.substring(separator + 1));
                    if (clientId != null && clientSecret != null) {
                        return new ClientCredentials(clientId, clientSecret);
                    }
                }
            } catch (IllegalArgumentException ignored) {
                return null;
            }
        }

        String clientId = normalize(form.getFirst("client_id"));
        String clientSecret = normalize(form.getFirst("client_secret"));
        if (clientId == null || clientSecret == null) {
            return null;
        }
        return new ClientCredentials(clientId, clientSecret);
    }

    /**
     * Executes the bearerToken operation.
     *
     * @param authorizationHeader Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String bearerToken(String authorizationHeader) {
        String normalized = normalize(authorizationHeader);
        if (normalized == null || !normalized.startsWith("Bearer ")) {
            return null;
        }
        String token = normalized.substring("Bearer ".length()).trim();
        return token.isEmpty() ? null : token;
    }

    /**
     * Executes the redirectWithError operation.
     *
     * @param redirectUri Parameter of type {@code String} used by this operation.
     * @param state Parameter of type {@code String} used by this operation.
     * @param errorCode Parameter of type {@code String} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private ResponseEntity<Void> redirectWithError(String redirectUri, String state, String errorCode) {
        String location = UriComponentsBuilder.fromUriString(redirectUri)
                .queryParam("error", errorCode)
                .queryParamIfPresent("state", Optional.ofNullable(normalize(state)))
                .build()
                .toUriString();
        return redirect(location);
    }

    /**
     * Executes the redirect operation.
     *
     * @param location Parameter of type {@code String} used by this operation.
     * @return {@code ResponseEntity<Void>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private ResponseEntity<Void> redirect(String location) {
        return ResponseEntity.status(HttpStatus.FOUND)
                .header(HttpHeaders.LOCATION, location)
                .build();
    }

    /**
     * Executes the oauthError operation.
     *
     * @param status Parameter of type {@code HttpStatus} used by this operation.
     * @param errorCode Parameter of type {@code String} used by this operation.
     * @return {@code ResponseEntity<Map<String, Object>>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private ResponseEntity<Map<String, Object>> oauthError(HttpStatus status, String errorCode) {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("error", errorCode);
        HttpHeaders headers = new HttpHeaders();
        if (status == HttpStatus.UNAUTHORIZED) {
            headers.set(HttpHeaders.WWW_AUTHENTICATE, "Bearer error=\"" + errorCode + "\"");
        }
        return new ResponseEntity<>(body, headers, status);
    }

    /**
     * Executes the cleanupExpiredGrants operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void cleanupExpiredGrants() {
        Instant now = Instant.now();
        authorizationCodes.entrySet().removeIf(entry -> entry.getValue().expiresAt().isBefore(now));
        accessTokens.entrySet().removeIf(entry -> entry.getValue().expiresAt().isBefore(now));
    }

    /**
     * Executes the requireDemoEnabled operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void requireDemoEnabled() {
        if (!demoEnabled) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND);
        }
    }

    /**
     * Executes the randomToken operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String randomToken() {
        return UUID.randomUUID().toString().replace("-", "");
    }

    /**
     * Executes the normalize operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalize(String value) {
        if (value == null) {
            return null;
        }
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    /**
     * Executes the normalizeOrDefault operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @param fallback Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalizeOrDefault(String value, String fallback) {
        String normalized = normalize(value);
        return normalized == null ? fallback : normalized;
    }

    private record AuthorizationCodeGrant(String clientId,
                                          String redirectUri,
                                          String scope,
                                          Map<String, Object> claims,
                                          Instant expiresAt) {
    }

    private record AccessTokenGrant(Map<String, Object> claims,
                                    String scope,
                                    Instant expiresAt) {
    }

    private record ClientCredentials(String clientId, String clientSecret) {
    }
}
