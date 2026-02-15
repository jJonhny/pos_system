package com.example.pos_system;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.MessageSource;
import org.springframework.test.context.ActiveProfiles;

import java.util.Locale;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@ActiveProfiles("test")
class I18nMessageSourceIntegrationTest {

    @Autowired
    private MessageSource messageSource;

    @Test
    void resolvesEnglishMessages() {
        String value = messageSource.getMessage("pos.checkout", null, Locale.ENGLISH);
        assertThat(value).isEqualTo("Checkout");
    }

    @Test
    void resolvesSimplifiedChineseMessages() {
        Locale zhCn = Locale.forLanguageTag("zh-CN");
        String value = messageSource.getMessage("pos.checkout", null, zhCn);
        assertThat(value).isEqualTo("结账");
    }

    @Test
    void resolvesChineseLanguageOnlyViaZhFallbackBundle() {
        Locale zh = Locale.forLanguageTag("zh");
        String value = messageSource.getMessage("pos.checkout", null, zh);
        assertThat(value).isEqualTo("结账");
    }

    @Test
    void fallsBackToEnglishForUnsupportedLocale() {
        Locale german = Locale.GERMAN;
        String value = messageSource.getMessage("pos.checkout", null, german);
        assertThat(value).isEqualTo("Checkout");
    }
}
