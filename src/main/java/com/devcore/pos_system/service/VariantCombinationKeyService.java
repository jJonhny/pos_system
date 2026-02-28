package com.devcore.pos_system.service;

import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

@Service
public class VariantCombinationKeyService {

    /**
     * Executes the canonicalKey operation.
     *
     * @param valuesByGroupCode Parameter of type {@code Map<String, String>} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String canonicalKey(Map<String, String> valuesByGroupCode) {
        if (valuesByGroupCode == null || valuesByGroupCode.isEmpty()) {
            return "";
        }
        Map<String, String> sorted = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
        valuesByGroupCode.forEach((group, value) -> {
            if (group == null || value == null) return;
            String g = group.trim();
            String v = value.trim();
            if (g.isEmpty() || v.isEmpty()) return;
            sorted.put(g, v);
        });
        return sorted.entrySet().stream()
                .map(e -> e.getKey() + "=" + e.getValue())
                .collect(Collectors.joining("|"));
    }

    /**
     * Executes the parseCanonicalKey operation.
     *
     * @param combinationKey Parameter of type {@code String} used by this operation.
     * @return {@code Map<String, String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Map<String, String> parseCanonicalKey(String combinationKey) {
        Map<String, String> values = new LinkedHashMap<>();
        if (combinationKey == null || combinationKey.isBlank()) {
            return values;
        }
        String[] tokens = combinationKey.split("\\|");
        for (String token : tokens) {
            int idx = token.indexOf('=');
            if (idx <= 0 || idx >= token.length() - 1) continue;
            String group = token.substring(0, idx).trim();
            String value = token.substring(idx + 1).trim();
            if (group.isEmpty() || value.isEmpty()) continue;
            values.put(group, value);
        }
        return new TreeMap<>(values);
    }

    /**
     * Executes the hash operation.
     *
     * @param canonicalKey Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String hash(String canonicalKey) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] out = digest.digest((canonicalKey == null ? "" : canonicalKey).getBytes(StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder(out.length * 2);
            for (byte b : out) {
                sb.append(String.format("%02x", b));
            }
            return sb.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("SHA-256 unavailable", e);
        }
    }
}
