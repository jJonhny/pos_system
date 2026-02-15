package com.example.pos_system.service;

import com.example.pos_system.entity.AppUser;
import com.example.pos_system.repository.AppUserRepo;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Locale;
import java.util.Optional;

@Service
public class UserLocalePreferenceService {
    public static final Locale LOCALE_EN = Locale.ENGLISH;
    public static final Locale LOCALE_ZH_CN = Locale.SIMPLIFIED_CHINESE;

    private final AppUserRepo appUserRepo;

    public UserLocalePreferenceService(AppUserRepo appUserRepo) {
        this.appUserRepo = appUserRepo;
    }

    public List<Locale> supportedLocales() {
        return List.of(LOCALE_EN, LOCALE_ZH_CN);
    }

    public Locale defaultLocale() {
        return LOCALE_EN;
    }

    public Optional<Locale> resolveCurrentUserPreferredLocale() {
        String username = currentUsername();
        if (username == null) return Optional.empty();
        return appUserRepo.findByUsername(username)
                .map(AppUser::getLanguagePreference)
                .map(this::parseSupportedLocale)
                .flatMap(Optional::ofNullable);
    }

    @Transactional
    public void persistCurrentUserPreference(Locale locale) {
        String username = currentUsername();
        if (username == null) return;
        Locale normalized = normalizeSupportedLocale(locale);
        if (normalized == null) return;
        appUserRepo.findByUsername(username).ifPresent(user -> {
            String next = toLanguageTag(normalized);
            if (!next.equalsIgnoreCase(user.getLanguagePreference())) {
                user.setLanguagePreference(next);
                appUserRepo.save(user);
            }
        });
    }

    public Locale parseSupportedLocale(String value) {
        if (value == null || value.isBlank()) return null;
        String normalized = value.trim().replace('_', '-').toLowerCase(Locale.ROOT);
        if ("zh".equals(normalized) || "zh-cn".equals(normalized) || "zh-hans-cn".equals(normalized)) {
            return LOCALE_ZH_CN;
        }
        if ("en".equals(normalized) || "en-us".equals(normalized) || "en-gb".equals(normalized)) {
            return LOCALE_EN;
        }
        Locale parsed = Locale.forLanguageTag(value);
        return normalizeSupportedLocale(parsed);
    }

    public Locale normalizeSupportedLocale(Locale locale) {
        if (locale == null) return null;
        String language = locale.getLanguage();
        String country = locale.getCountry();
        if ("zh".equalsIgnoreCase(language)) {
            if ("CN".equalsIgnoreCase(country) || country == null || country.isBlank()) {
                return LOCALE_ZH_CN;
            }
            return LOCALE_ZH_CN;
        }
        if ("en".equalsIgnoreCase(language)) {
            return LOCALE_EN;
        }
        return null;
    }

    public String toLanguageTag(Locale locale) {
        Locale normalized = normalizeSupportedLocale(locale);
        if (normalized == null) return defaultLocale().toLanguageTag();
        if (LOCALE_ZH_CN.equals(normalized)) return "zh-CN";
        return normalized.toLanguageTag();
    }

    private String currentUsername() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !authentication.isAuthenticated()) return null;
        String name = authentication.getName();
        if (name == null || "anonymousUser".equalsIgnoreCase(name)) return null;
        return name;
    }
}
