package com.example.pos_system.service;

import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;

import java.util.Locale;

@Service
public class I18nService {
    private final MessageSource messageSource;
    private final UserLocalePreferenceService userLocalePreferenceService;

    public I18nService(MessageSource messageSource,
                       UserLocalePreferenceService userLocalePreferenceService) {
        this.messageSource = messageSource;
        this.userLocalePreferenceService = userLocalePreferenceService;
    }

    public String msg(String key, Object... args) {
        return msg(LocaleContextHolder.getLocale(), key, args);
    }

    public String msg(Locale locale, String key, Object... args) {
        Locale effective = userLocalePreferenceService.normalizeSupportedLocale(locale);
        if (effective == null) {
            effective = userLocalePreferenceService.defaultLocale();
        }
        return messageSource.getMessage(key, args, key, effective);
    }

    public Locale parseOrDefault(String localeTag) {
        Locale parsed = userLocalePreferenceService.parseSupportedLocale(localeTag);
        return parsed == null ? userLocalePreferenceService.defaultLocale() : parsed;
    }
}
