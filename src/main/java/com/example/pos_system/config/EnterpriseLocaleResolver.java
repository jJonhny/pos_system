package com.example.pos_system.config;

import com.example.pos_system.service.UserLocalePreferenceService;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.web.servlet.LocaleResolver;

import java.util.Enumeration;
import java.util.Locale;

public class EnterpriseLocaleResolver implements LocaleResolver {
    private static final String LOCALE_ATTR = EnterpriseLocaleResolver.class.getName() + ".RESOLVED_LOCALE";
    private static final String LOCALE_COOKIE = "POS_LANG";
    private static final int COOKIE_MAX_AGE_SECONDS = 60 * 60 * 24 * 365;

    private final UserLocalePreferenceService userLocalePreferenceService;

    public EnterpriseLocaleResolver(UserLocalePreferenceService userLocalePreferenceService) {
        this.userLocalePreferenceService = userLocalePreferenceService;
    }

    @Override
    public Locale resolveLocale(HttpServletRequest request) {
        Object attr = request.getAttribute(LOCALE_ATTR);
        if (attr instanceof Locale locale) {
            return locale;
        }

        Locale locale = userLocalePreferenceService.resolveCurrentUserPreferredLocale().orElse(null);
        if (locale == null) {
            locale = resolveCookieLocale(request);
        }
        if (locale == null) {
            locale = userLocalePreferenceService.parseSupportedLocale(request.getParameter("lang"));
        }
        if (locale == null) {
            locale = resolveAcceptLanguageLocale(request);
        }
        if (locale == null) {
            locale = userLocalePreferenceService.defaultLocale();
        }

        request.setAttribute(LOCALE_ATTR, locale);
        return locale;
    }

    @Override
    public void setLocale(HttpServletRequest request, HttpServletResponse response, Locale locale) {
        Locale resolved = userLocalePreferenceService.normalizeSupportedLocale(locale);
        if (resolved == null) {
            resolved = userLocalePreferenceService.defaultLocale();
        }
        request.setAttribute(LOCALE_ATTR, resolved);
        LocaleContextHolder.setLocale(resolved);
        writeLocaleCookie(response, resolved, request.isSecure());
        userLocalePreferenceService.persistCurrentUserPreference(resolved);
    }

    private Locale resolveCookieLocale(HttpServletRequest request) {
        Cookie[] cookies = request.getCookies();
        if (cookies == null || cookies.length == 0) return null;
        for (Cookie cookie : cookies) {
            if (!LOCALE_COOKIE.equals(cookie.getName())) continue;
            Locale locale = userLocalePreferenceService.parseSupportedLocale(cookie.getValue());
            if (locale != null) {
                return locale;
            }
        }
        return null;
    }

    private Locale resolveAcceptLanguageLocale(HttpServletRequest request) {
        Enumeration<Locale> locales = request.getLocales();
        if (locales == null) return null;
        while (locales.hasMoreElements()) {
            Locale candidate = userLocalePreferenceService.normalizeSupportedLocale(locales.nextElement());
            if (candidate != null) {
                return candidate;
            }
        }
        return null;
    }

    private void writeLocaleCookie(HttpServletResponse response, Locale locale, boolean secure) {
        if (response == null) return;
        Cookie cookie = new Cookie(LOCALE_COOKIE, userLocalePreferenceService.toLanguageTag(locale));
        cookie.setPath("/");
        cookie.setHttpOnly(false);
        cookie.setSecure(secure);
        cookie.setMaxAge(COOKIE_MAX_AGE_SECONDS);
        response.addCookie(cookie);
    }
}
