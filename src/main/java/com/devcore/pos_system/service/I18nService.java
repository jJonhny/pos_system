package com.devcore.pos_system.service;

import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;

import java.util.Locale;

@Service
public class I18nService {
    private final MessageSource messageSource;
    private final UserLocalePreferenceService userLocalePreferenceService;

    /**
     * Executes the I18nService operation.
     * <p>Return value: A fully initialized I18nService instance.</p>
     *
     * @param messageSource Parameter of type {@code MessageSource} used by this operation.
     * @param userLocalePreferenceService Parameter of type {@code UserLocalePreferenceService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public I18nService(MessageSource messageSource,
                       UserLocalePreferenceService userLocalePreferenceService) {
        this.messageSource = messageSource;
        this.userLocalePreferenceService = userLocalePreferenceService;
    }

    /**
     * Executes the msg operation.
     *
     * @param key Parameter of type {@code String} used by this operation.
     * @param args Parameter of type {@code Object...} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String msg(String key, Object... args) {
        return msg(LocaleContextHolder.getLocale(), key, args);
    }

    /**
     * Executes the msg operation.
     *
     * @param locale Parameter of type {@code Locale} used by this operation.
     * @param key Parameter of type {@code String} used by this operation.
     * @param args Parameter of type {@code Object...} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String msg(Locale locale, String key, Object... args) {
        Locale effective = userLocalePreferenceService.normalizeSupportedLocale(locale);
        if (effective == null) {
            effective = userLocalePreferenceService.defaultLocale();
        }
        return messageSource.getMessage(key, args, key, effective);
    }

    /**
     * Executes the parseOrDefault operation.
     *
     * @param localeTag Parameter of type {@code String} used by this operation.
     * @return {@code Locale} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Locale parseOrDefault(String localeTag) {
        Locale parsed = userLocalePreferenceService.parseSupportedLocale(localeTag);
        return parsed == null ? userLocalePreferenceService.defaultLocale() : parsed;
    }
}
