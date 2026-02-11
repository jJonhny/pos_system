package com.example.pos_system.config;

import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;

public class LegacyUpgradingPasswordEncoder implements PasswordEncoder {
    private final BCryptPasswordEncoder bcrypt = new BCryptPasswordEncoder();

    @Override
    public String encode(CharSequence rawPassword) {
        return bcrypt.encode(rawPassword);
    }

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

    @Override
    public boolean upgradeEncoding(String encodedPassword) {
        if (encodedPassword == null) return false;
        if (encodedPassword.startsWith("{bcrypt}")) return false;
        return !isBcryptHash(encodedPassword);
    }

    private boolean isBcryptHash(String encodedPassword) {
        return encodedPassword.startsWith("$2a$")
                || encodedPassword.startsWith("$2b$")
                || encodedPassword.startsWith("$2y$");
    }
}
