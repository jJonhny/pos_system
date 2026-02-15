package com.example.pos_system.config;

import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.service.UserLocalePreferenceService;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ModelAttribute;

import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

@ControllerAdvice
public class GlobalModelAttributes {
    private final ProductRepo productRepo;
    private final UserLocalePreferenceService userLocalePreferenceService;

    public GlobalModelAttributes(ProductRepo productRepo,
                                 UserLocalePreferenceService userLocalePreferenceService) {
        this.productRepo = productRepo;
        this.userLocalePreferenceService = userLocalePreferenceService;
    }

    @ModelAttribute("lowStockCount")
    public long lowStockCount() {
        return productRepo.countLowStock();
    }

    @ModelAttribute("currentLang")
    public String currentLang(Locale locale) {
        return userLocalePreferenceService.toLanguageTag(locale);
    }

    @ModelAttribute("supportedLangs")
    public Map<String, String> supportedLangs() {
        Map<String, String> options = new LinkedHashMap<>();
        options.put("en", "English");
        options.put("zh-CN", "中文");
        return options;
    }
}
