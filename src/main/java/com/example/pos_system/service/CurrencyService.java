package com.example.pos_system.service;

import com.example.pos_system.entity.Currency;
import com.example.pos_system.repository.CurrencyRateLogRepo;
import com.example.pos_system.repository.CurrencyRepo;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;
import org.springframework.scheduling.annotation.Scheduled;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Service
public class CurrencyService {
    private static final Logger log = LoggerFactory.getLogger(CurrencyService.class);
    private final CurrencyRepo currencyRepo;
    private final CurrencyRateLogRepo rateLogRepo;
    private final RestTemplate restTemplate = new RestTemplate();
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Value("${app.currency.base:USD}")
    private String defaultBaseCode;

    @Value("${app.currency.base-name:US Dollar}")
    private String defaultBaseName;

    @Value("${app.currency.base-symbol:$}")
    private String defaultBaseSymbol;

    @Value("${app.currency.base-decimals:2}")
    private int defaultBaseDecimals;

    @Value("${app.currency.rate-url:}")
    private String rateUrl;

    @Value("${app.currency.rate-path:rates}")
    private String ratePath;

    private volatile Currency baseCache;

    public CurrencyService(CurrencyRepo currencyRepo, CurrencyRateLogRepo rateLogRepo) {
        this.currencyRepo = currencyRepo;
        this.rateLogRepo = rateLogRepo;
    }

    @Transactional
    public Currency getBaseCurrency() {
        Currency cached = baseCache;
        if (cached != null) return cached;
        Currency base = ensureBaseCurrency();
        baseCache = base;
        return base;
    }

    @Transactional
    public Currency ensureBaseCurrency() {
        Optional<Currency> existingBase = currencyRepo.findByBaseTrue();
        if (existingBase.isPresent()) {
            Currency base = existingBase.get();
            baseCache = base;
            return base;
        }
        String code = normalizeCode(defaultBaseCode);
        Currency base = currencyRepo.findByCodeIgnoreCase(code).orElseGet(Currency::new);
        base.setCode(code);
        if (base.getName() == null || base.getName().isBlank()) {
            base.setName(defaultBaseName);
        }
        if (base.getSymbol() == null || base.getSymbol().isBlank()) {
            base.setSymbol(defaultBaseSymbol);
        }
        base.setRateToBase(BigDecimal.ONE);
        base.setBase(true);
        base.setActive(true);
        base.setFractionDigits(defaultBaseDecimals);
        base.setUpdatedAt(LocalDateTime.now());
        Currency saved = currencyRepo.save(base);
        baseCache = saved;
        return saved;
    }

    @Transactional(readOnly = true)
    public List<Currency> getActiveCurrencies() {
        List<Currency> active = new ArrayList<>(currencyRepo.findByActiveTrueOrderByCodeAsc());
        Currency base = getBaseCurrency();
        if (active.stream().noneMatch(c -> c.getId().equals(base.getId()))) {
            active.add(base);
        }
        active.sort(Comparator.comparing((Currency c) -> !Boolean.TRUE.equals(c.getBase()))
                .thenComparing(Currency::getCode));
        return active;
    }

    @Transactional(readOnly = true)
    public List<Currency> getAllCurrencies() {
        return currencyRepo.findAllByOrderByCodeAsc();
    }

    @Transactional
    public Currency createCurrency(String code, String name, String symbol, BigDecimal rateToBase,
                                   Integer fractionDigits, Boolean active) {
        Currency currency = new Currency();
        currency.setCode(normalizeCode(code));
        currency.setName(name == null ? "" : name.trim());
        currency.setSymbol(symbol == null ? null : symbol.trim());
        currency.setRateToBase(rateToBase == null ? BigDecimal.ONE : rateToBase);
        currency.setFractionDigits(fractionDigits == null ? 2 : Math.max(0, fractionDigits));
        currency.setActive(active == null || active);
        currency.setBase(false);
        currency.setUpdatedAt(LocalDateTime.now());
        Currency saved = currencyRepo.save(currency);
        recordRateLog(saved);
        return saved;
    }

    @Transactional
    public Currency updateCurrency(Long id, String name, String symbol, BigDecimal rateToBase,
                                   Integer fractionDigits, Boolean active) {
        Currency currency = currencyRepo.findById(id).orElse(null);
        if (currency == null) return null;
        if (name != null) currency.setName(name.trim());
        if (symbol != null) currency.setSymbol(symbol.trim());
        if (rateToBase != null) currency.setRateToBase(rateToBase);
        if (fractionDigits != null) currency.setFractionDigits(Math.max(0, fractionDigits));
        if (active != null && !Boolean.TRUE.equals(currency.getBase())) currency.setActive(active);
        currency.setUpdatedAt(LocalDateTime.now());
        Currency saved = currencyRepo.save(currency);
        recordRateLog(saved);
        return saved;
    }

    @Transactional
    public boolean setBaseCurrency(Long id) {
        Currency newBase = currencyRepo.findById(id).orElse(null);
        if (newBase == null) return false;
        Currency oldBase = currencyRepo.findByBaseTrue().orElse(null);
        BigDecimal factor = newBase.getRateToBase();
        if (factor == null || factor.compareTo(BigDecimal.ZERO) <= 0) {
            factor = BigDecimal.ONE;
        }
        List<Currency> all = currencyRepo.findAll();
        for (Currency currency : all) {
            boolean isBase = currency.getId().equals(newBase.getId());
            currency.setBase(isBase);
            if (isBase) {
                currency.setActive(true);
                currency.setRateToBase(BigDecimal.ONE);
            } else if (currency.getRateToBase() != null && factor.compareTo(BigDecimal.ZERO) > 0) {
                currency.setRateToBase(currency.getRateToBase().divide(factor, 8, RoundingMode.HALF_UP));
            }
            currency.setUpdatedAt(LocalDateTime.now());
        }
        currencyRepo.saveAll(all);
        baseCache = newBase;
        if (oldBase != null && !oldBase.getId().equals(newBase.getId())) {
            recordRateLog(oldBase);
        }
        recordRateLog(newBase);
        return true;
    }

    @Transactional
    public int refreshRates() {
        if (rateUrl == null || rateUrl.isBlank()) {
            return 0;
        }
        Currency base = getBaseCurrency();
        String url = rateUrl.replace("{base}", base.getCode());
        String payload;
        try {
            payload = restTemplate.getForObject(url, String.class);
        } catch (RestClientException ex) {
            throw new IllegalStateException("Failed to fetch rates: " + ex.getMessage(), ex);
        }
        if (payload == null || payload.isBlank()) {
            throw new IllegalStateException("Rate provider returned empty response.");
        }
        Map<String, BigDecimal> rates = parseRates(payload);
        if (rates.isEmpty()) {
            throw new IllegalStateException("Rate provider response missing rates.");
        }
        int updated = 0;
        List<com.example.pos_system.entity.CurrencyRateLog> logs = new ArrayList<>();
        List<Currency> all = currencyRepo.findAll();
        for (Currency currency : all) {
            if (Boolean.TRUE.equals(currency.getBase())) {
                currency.setRateToBase(BigDecimal.ONE);
                currency.setUpdatedAt(LocalDateTime.now());
                continue;
            }
            BigDecimal rateFromBase = rates.get(currency.getCode().toUpperCase());
            if (rateFromBase == null || rateFromBase.compareTo(BigDecimal.ZERO) <= 0) continue;
            BigDecimal rateToBase = BigDecimal.ONE.divide(rateFromBase, 8, RoundingMode.HALF_UP);
            currency.setRateToBase(rateToBase);
            currency.setUpdatedAt(LocalDateTime.now());
            updated++;
            logs.add(buildRateLog(currency.getCode(), rateToBase));
        }
        currencyRepo.saveAll(all);
        if (!logs.isEmpty()) {
            rateLogRepo.saveAll(logs);
        }
        return updated;
    }

    @Transactional(readOnly = true)
    public Currency findByCode(String code) {
        if (code == null) return null;
        return currencyRepo.findByCodeIgnoreCase(code.trim()).orElse(null);
    }

    @Scheduled(fixedDelayString = "${app.currency.refresh-ms:900000}",
            initialDelayString = "${app.currency.refresh-initial-ms:30000}")
    public void scheduledRefresh() {
        if (rateUrl == null || rateUrl.isBlank()) return;
        try {
            int updated = refreshRates();
            if (updated > 0) {
                log.info("Currency rates refreshed. Updated={}", updated);
            }
        } catch (Exception ex) {
            log.warn("Currency rate refresh failed: {}", ex.getMessage());
        }
    }

    private Map<String, BigDecimal> parseRates(String payload) {
        try {
            JsonNode root = objectMapper.readTree(payload);
            JsonNode ratesNode = root.path(ratePath);
            if (!ratesNode.isObject()) {
                return Map.of();
            }
            Map<String, BigDecimal> rates = new HashMap<>();
            ratesNode.fields().forEachRemaining(entry -> {
                JsonNode value = entry.getValue();
                if (value != null && value.isNumber()) {
                    rates.put(entry.getKey().toUpperCase(), value.decimalValue());
                }
            });
            return rates;
        } catch (Exception ex) {
            throw new IllegalStateException("Failed to parse rates response.", ex);
        }
    }

    private String normalizeCode(String code) {
        return code == null ? "" : code.trim().toUpperCase();
    }

    private void recordRateLog(Currency currency) {
        if (currency == null || currency.getRateToBase() == null || currency.getCode() == null) return;
        rateLogRepo.save(buildRateLog(currency.getCode(), currency.getRateToBase()));
    }

    private com.example.pos_system.entity.CurrencyRateLog buildRateLog(String code, BigDecimal rateToBase) {
        com.example.pos_system.entity.CurrencyRateLog log = new com.example.pos_system.entity.CurrencyRateLog();
        log.setCurrencyCode(code);
        log.setRateToBase(rateToBase);
        log.setCreatedAt(LocalDateTime.now());
        return log;
    }
}
