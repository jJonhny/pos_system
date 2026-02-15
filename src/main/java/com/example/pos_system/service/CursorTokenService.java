package com.example.pos_system.service;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.Instant;
import java.util.Base64;

@Service
public class CursorTokenService {
    private static final Duration CURSOR_TTL = Duration.ofHours(4);
    private static final Base64.Encoder URL_ENCODER = Base64.getUrlEncoder().withoutPadding();
    private static final Base64.Decoder URL_DECODER = Base64.getUrlDecoder();

    @Value("${app.pagination.cursor-secret:pos-cursor-secret-change-me}")
    private String cursorSecret;

    public String createProductCursor(Long lastId, String query, Long categoryId) {
        if (lastId == null || lastId <= 0) {
            throw new IllegalArgumentException("Cannot create cursor without a valid product id.");
        }
        String fingerprint = filterFingerprint(query, categoryId);
        long expiresAt = Instant.now().plus(CURSOR_TTL).getEpochSecond();
        String payload = "v1|" + lastId + "|" + fingerprint + "|" + expiresAt;
        String payloadEncoded = URL_ENCODER.encodeToString(payload.getBytes(StandardCharsets.UTF_8));
        String signature = URL_ENCODER.encodeToString(hmac(payload));
        return payloadEncoded + "." + signature;
    }

    public Long parseProductCursor(String token, String query, Long categoryId) {
        if (token == null || token.isBlank()) return null;
        String[] parts = token.split("\\.");
        if (parts.length != 2) {
            throw new IllegalArgumentException("Invalid cursor format.");
        }
        String payload = decodePart(parts[0], "payload");
        byte[] expectedSig = hmac(payload);
        byte[] actualSig = decodeBytes(parts[1], "signature");
        if (!MessageDigest.isEqual(expectedSig, actualSig)) {
            throw new IllegalArgumentException("Invalid cursor signature.");
        }

        String[] values = payload.split("\\|");
        if (values.length != 4 || !"v1".equals(values[0])) {
            throw new IllegalArgumentException("Unsupported cursor version.");
        }

        long lastId;
        long expiresAt;
        try {
            lastId = Long.parseLong(values[1]);
            expiresAt = Long.parseLong(values[3]);
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException("Malformed cursor payload.");
        }
        if (lastId <= 0) {
            throw new IllegalArgumentException("Cursor id must be positive.");
        }
        if (Instant.now().getEpochSecond() > expiresAt) {
            throw new IllegalArgumentException("Cursor expired.");
        }

        String expectedFingerprint = filterFingerprint(query, categoryId);
        if (!expectedFingerprint.equals(values[2])) {
            throw new IllegalArgumentException("Cursor does not match the active filters.");
        }
        return lastId;
    }

    public String filterFingerprint(String query, Long categoryId) {
        String normalizedQuery = query == null ? "" : query.trim().toLowerCase();
        String category = categoryId == null ? "" : String.valueOf(categoryId);
        String value = normalizedQuery + "|" + category;
        return shortHash(value);
    }

    private byte[] hmac(String payload) {
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            mac.init(new SecretKeySpec(cursorSecret.getBytes(StandardCharsets.UTF_8), "HmacSHA256"));
            return mac.doFinal(payload.getBytes(StandardCharsets.UTF_8));
        } catch (Exception ex) {
            throw new IllegalStateException("Unable to sign cursor token.", ex);
        }
    }

    private String shortHash(String value) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] bytes = digest.digest(value.getBytes(StandardCharsets.UTF_8));
            StringBuilder out = new StringBuilder();
            for (int i = 0; i < 8 && i < bytes.length; i++) {
                out.append(String.format("%02x", bytes[i]));
            }
            return out.toString();
        } catch (NoSuchAlgorithmException ex) {
            throw new IllegalStateException("Unable to hash cursor fingerprint.", ex);
        }
    }

    private String decodePart(String value, String label) {
        return new String(decodeBytes(value, label), StandardCharsets.UTF_8);
    }

    private byte[] decodeBytes(String value, String label) {
        try {
            return URL_DECODER.decode(value);
        } catch (IllegalArgumentException ex) {
            throw new IllegalArgumentException("Invalid cursor " + label + ".");
        }
    }
}
