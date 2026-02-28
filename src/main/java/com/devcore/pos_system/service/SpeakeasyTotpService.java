package com.devcore.pos_system.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

@Service
public class SpeakeasyTotpService {
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Value("${app.security.totp.node-command:node}")
    private String nodeCommand;

    @Value("${app.security.totp.script-path:scripts/dev/speakeasy-totp.js}")
    private String scriptPath;

    @Value("${app.security.totp.issuer:DevCore POS}")
    private String issuer;

    @Value("${app.security.totp.verify-window:2}")
    private int verifyWindow;

    /**
     * Executes the generateSetup operation.
     *
     * @param label Parameter of type {@code String} used by this operation.
     * @return {@code SetupPayload} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public SetupPayload generateSetup(String label) {
        JsonNode node = runNodeCommand("setup", label, issuer);
        String base32 = text(node, "base32");
        String otpauthUrl = text(node, "otpauthUrl");
        String qrDataUrl = text(node, "qrDataUrl");
        if (base32 == null || otpauthUrl == null || qrDataUrl == null) {
            throw new IllegalStateException("Invalid response from Speakeasy setup command.");
        }
        return new SetupPayload(base32, otpauthUrl, qrDataUrl);
    }

    /**
     * Executes the buildSetupFromSecret operation.
     *
     * @param base32Secret Parameter of type {@code String} used by this operation.
     * @param label Parameter of type {@code String} used by this operation.
     * @return {@code SetupPayload} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public SetupPayload buildSetupFromSecret(String base32Secret, String label) {
        String normalizedSecret = base32Secret == null ? "" : base32Secret.trim();
        if (normalizedSecret.isEmpty()) {
            throw new IllegalArgumentException("Base32 secret is required.");
        }
        JsonNode node = runNodeCommand("render", normalizedSecret, label, issuer);
        String otpauthUrl = text(node, "otpauthUrl");
        String qrDataUrl = text(node, "qrDataUrl");
        if (otpauthUrl == null || qrDataUrl == null) {
            throw new IllegalStateException("Invalid response from Speakeasy render command.");
        }
        return new SetupPayload(normalizedSecret, otpauthUrl, qrDataUrl);
    }

    /**
     * Executes the verifyCode operation.
     *
     * @param base32Secret Parameter of type {@code String} used by this operation.
     * @param code Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public boolean verifyCode(String base32Secret, String code) {
        if (base32Secret == null || base32Secret.isBlank()) {
            return false;
        }
        String normalizedCode = code == null ? "" : code.trim();
        if (!normalizedCode.matches("\\d{6}")) {
            return false;
        }
        JsonNode node = runNodeCommand("verify", base32Secret, normalizedCode, String.valueOf(normalizedVerifyWindow()));
        return node.path("valid").asBoolean(false);
    }

    /**
     * Executes the runNodeCommand operation.
     *
     * @param args Parameter of type {@code String...} used by this operation.
     * @return {@code JsonNode} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private JsonNode runNodeCommand(String... args) {
        List<String> command = new ArrayList<>();
        command.add(nodeCommand);
        command.add(resolveScriptPath());
        for (String arg : args) {
            command.add(arg);
        }

        ProcessBuilder processBuilder = new ProcessBuilder(command);
        processBuilder.redirectErrorStream(false);
        try {
            Process process = processBuilder.start();
            String stdout = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
            String stderr = new String(process.getErrorStream().readAllBytes(), StandardCharsets.UTF_8).trim();
            int exit = process.waitFor();
            if (exit != 0) {
                String reason = stderr.isBlank() ? stdout : stderr;
                throw new IllegalStateException("Speakeasy command failed: " + reason);
            }
            if (stdout.isBlank()) {
                throw new IllegalStateException("Speakeasy command returned empty output.");
            }
            return objectMapper.readTree(stdout);
        } catch (IOException ex) {
            throw new IllegalStateException("Unable to execute Speakeasy node command.", ex);
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("Interrupted while executing Speakeasy node command.", ex);
        }
    }

    /**
     * Executes the resolveScriptPath operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String resolveScriptPath() {
        Path path = Path.of(scriptPath);
        if (path.isAbsolute()) {
            return path.toString();
        }
        return Path.of("").toAbsolutePath().resolve(path).normalize().toString();
    }

    /**
     * Executes the text operation.
     *
     * @param node Parameter of type {@code JsonNode} used by this operation.
     * @param field Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String text(JsonNode node, String field) {
        if (node == null || node.path(field).isMissingNode()) {
            return null;
        }
        String value = node.path(field).asText(null);
        if (value == null || value.isBlank()) {
            return null;
        }
        return value;
    }

    /**
     * Executes the normalizedVerifyWindow operation.
     *
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int normalizedVerifyWindow() {
        if (verifyWindow < 0) {
            return 0;
        }
        return Math.min(verifyWindow, 5);
    }

    public record SetupPayload(String base32Secret, String otpauthUrl, String qrDataUrl) {
    }
}
