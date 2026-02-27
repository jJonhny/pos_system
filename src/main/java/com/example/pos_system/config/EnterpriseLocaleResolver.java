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

    /**
     * Executes the EnterpriseLocaleResolver operation.
     * <p>Return value: A fully initialized EnterpriseLocaleResolver instance.</p>
     *
     * @param userLocalePreferenceService Parameter of type {@code UserLocalePreferenceService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public EnterpriseLocaleResolver(UserLocalePreferenceService userLocalePreferenceService) {
        this.userLocalePreferenceService = userLocalePreferenceService;
    }

    /**
     * Executes the resolveLocale operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code Locale} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the resolveLocale operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code Locale} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the resolveLocale operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code Locale} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the setLocale operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param locale Parameter of type {@code Locale} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the setLocale operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param locale Parameter of type {@code Locale} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the setLocale operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param locale Parameter of type {@code Locale} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the resolveCookieLocale operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code Locale} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the resolveAcceptLanguageLocale operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code Locale} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the writeLocaleCookie operation.
     *
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param locale Parameter of type {@code Locale} used by this operation.
     * @param secure Parameter of type {@code boolean} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
