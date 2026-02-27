package com.example.pos_system.service;

import com.example.pos_system.entity.AppUser;
import com.example.pos_system.entity.Permission;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.crypto.SecretKey;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Set;

@Service
public class JwtTokenService {
    private static final String CLAIM_TYPE = "type";
    private static final String TYPE_ACCESS = "ACCESS";
    private static final String TYPE_OTP_CHALLENGE = "OTP_CHALLENGE";

    @Value("${app.security.jwt.secret}")
    private String jwtSecret;

    @Value("${app.security.jwt.access-token-minutes:720}")
    private long accessTokenMinutes;

    @Value("${app.security.jwt.challenge-token-minutes:5}")
    private long challengeTokenMinutes;

    /**
     * Executes the issueOtpChallengeToken operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String issueOtpChallengeToken(AppUser user) {
        Instant now = Instant.now();
        Instant exp = now.plus(normalizeMinutes(challengeTokenMinutes), ChronoUnit.MINUTES);
        return Jwts.builder()
                .subject(String.valueOf(user.getId()))
                .claim(CLAIM_TYPE, TYPE_OTP_CHALLENGE)
                .claim("username", user.getUsername())
                .claim("email", user.getEmail())
                .issuedAt(java.util.Date.from(now))
                .expiration(java.util.Date.from(exp))
                .signWith(secretKey())
                .compact();
    }

    /**
     * Executes the issueAccessToken operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String issueAccessToken(AppUser user) {
        Instant now = Instant.now();
        Instant exp = now.plus(normalizeMinutes(accessTokenMinutes), ChronoUnit.MINUTES);
        Set<Permission> permissions = user.getPermissions();
        List<String> permissionNames = permissions == null ? List.of() : permissions.stream().map(Enum::name).sorted().toList();
        return Jwts.builder()
                .subject(String.valueOf(user.getId()))
                .claim(CLAIM_TYPE, TYPE_ACCESS)
                .claim("username", user.getUsername())
                .claim("email", user.getEmail())
                .claim("role", user.getRole() == null ? null : user.getRole().name())
                .claim("permissions", permissionNames)
                .issuedAt(java.util.Date.from(now))
                .expiration(java.util.Date.from(exp))
                .signWith(secretKey())
                .compact();
    }

    /**
     * Executes the parseAccessToken operation.
     *
     * @param token Parameter of type {@code String} used by this operation.
     * @return {@code Claims} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Claims parseAccessToken(String token) {
        Claims claims = parseToken(token);
        ensureType(claims, TYPE_ACCESS);
        return claims;
    }

    /**
     * Executes the parseOtpChallengeToken operation.
     *
     * @param token Parameter of type {@code String} used by this operation.
     * @return {@code Claims} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Claims parseOtpChallengeToken(String token) {
        Claims claims = parseToken(token);
        ensureType(claims, TYPE_OTP_CHALLENGE);
        return claims;
    }

    /**
     * Executes the parseToken operation.
     *
     * @param token Parameter of type {@code String} used by this operation.
     * @return {@code Claims} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Claims parseToken(String token) {
        if (token == null || token.isBlank()) {
            throw new IllegalArgumentException("Token is required.");
        }
        return Jwts.parser()
                .verifyWith(secretKey())
                .build()
                .parseSignedClaims(token.trim())
                .getPayload();
    }

    /**
     * Executes the ensureType operation.
     *
     * @param claims Parameter of type {@code Claims} used by this operation.
     * @param expectedType Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void ensureType(Claims claims, String expectedType) {
        String type = claims.get(CLAIM_TYPE, String.class);
        if (!expectedType.equals(type)) {
            throw new IllegalArgumentException("Unexpected token type.");
        }
    }

    /**
     * Executes the secretKey operation.
     *
     * @return {@code SecretKey} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private SecretKey secretKey() {
        String secret = jwtSecret == null ? "" : jwtSecret.trim();
        byte[] keyBytes = secret.getBytes(StandardCharsets.UTF_8);
        if (keyBytes.length < 32) {
            throw new IllegalStateException("JWT secret must be at least 32 bytes.");
        }
        return Keys.hmacShaKeyFor(keyBytes);
    }

    /**
     * Executes the normalizeMinutes operation.
     *
     * @param configured Parameter of type {@code long} used by this operation.
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private long normalizeMinutes(long configured) {
        return configured <= 0 ? 5 : configured;
    }
}
