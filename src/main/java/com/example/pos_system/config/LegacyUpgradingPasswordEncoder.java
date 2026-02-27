package com.example.pos_system.config;

import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;

public class LegacyUpgradingPasswordEncoder implements PasswordEncoder {
    private final BCryptPasswordEncoder bcrypt = new BCryptPasswordEncoder();

    /**
     * Executes the encode operation.
     *
     * @param rawPassword Parameter of type {@code CharSequence} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the encode operation.
     *
     * @param rawPassword Parameter of type {@code CharSequence} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the encode operation.
     *
     * @param rawPassword Parameter of type {@code CharSequence} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Override
    public String encode(CharSequence rawPassword) {
        return bcrypt.encode(rawPassword);
    }

    /**
     * Executes the matches operation.
     *
     * @param rawPassword Parameter of type {@code CharSequence} used by this operation.
     * @param encodedPassword Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the matches operation.
     *
     * @param rawPassword Parameter of type {@code CharSequence} used by this operation.
     * @param encodedPassword Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the matches operation.
     *
     * @param rawPassword Parameter of type {@code CharSequence} used by this operation.
     * @param encodedPassword Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Override
    public boolean matches(CharSequence rawPassword, String encodedPassword) {
        if (encodedPassword == null) return false;
        if (encodedPassword.startsWith("{bcrypt}")) {
            String hash = encodedPassword.substring("{bcrypt}".length());
            return bcrypt.matches(rawPassword, hash);
        }
        if (encodedPassword.startsWith("{noop}")) {
            String raw = encodedPassword.substring("{noop}".length());
            return rawPassword != null && rawPassword.toString().equals(raw);
        }
        if (isBcryptHash(encodedPassword)) {
            return bcrypt.matches(rawPassword, encodedPassword);
        }
        return rawPassword != null && rawPassword.toString().equals(encodedPassword);
    }

    /**
     * Executes the upgradeEncoding operation.
     *
     * @param encodedPassword Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the upgradeEncoding operation.
     *
     * @param encodedPassword Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the upgradeEncoding operation.
     *
     * @param encodedPassword Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Override
    public boolean upgradeEncoding(String encodedPassword) {
        if (encodedPassword == null) return false;
        if (encodedPassword.startsWith("{bcrypt}")) return false;
        return !isBcryptHash(encodedPassword);
    }

    /**
     * Executes the isBcryptHash operation.
     *
     * @param encodedPassword Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean isBcryptHash(String encodedPassword) {
        return encodedPassword.startsWith("$2a$")
                || encodedPassword.startsWith("$2b$")
                || encodedPassword.startsWith("$2y$");
    }
}
