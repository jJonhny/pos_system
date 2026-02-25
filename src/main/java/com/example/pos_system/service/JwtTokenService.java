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

    @Value("${app.security.jwt.secret:change-this-secret-key-with-at-least-32-bytes}")
    private String jwtSecret;

    @Value("${app.security.jwt.access-token-minutes:720}")
    private long accessTokenMinutes;

    @Value("${app.security.jwt.challenge-token-minutes:5}")
    private long challengeTokenMinutes;

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

    public Claims parseAccessToken(String token) {
        Claims claims = parseToken(token);
        ensureType(claims, TYPE_ACCESS);
        return claims;
    }

    public Claims parseOtpChallengeToken(String token) {
        Claims claims = parseToken(token);
        ensureType(claims, TYPE_OTP_CHALLENGE);
        return claims;
    }

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

    private void ensureType(Claims claims, String expectedType) {
        String type = claims.get(CLAIM_TYPE, String.class);
        if (!expectedType.equals(type)) {
            throw new IllegalArgumentException("Unexpected token type.");
        }
    }

    private SecretKey secretKey() {
        String secret = jwtSecret == null ? "" : jwtSecret.trim();
        byte[] keyBytes = secret.getBytes(StandardCharsets.UTF_8);
        if (keyBytes.length < 32) {
            throw new IllegalStateException("JWT secret must be at least 32 bytes.");
        }
        return Keys.hmacShaKeyFor(keyBytes);
    }

    private long normalizeMinutes(long configured) {
        return configured <= 0 ? 5 : configured;
    }
}
