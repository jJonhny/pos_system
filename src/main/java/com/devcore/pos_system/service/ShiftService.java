package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.PaymentMethod;
import com.devcore.pos_system.entity.Sale;
import com.devcore.pos_system.entity.SalePayment;
import com.devcore.pos_system.entity.SaleStatus;
import com.devcore.pos_system.entity.Shift;
import com.devcore.pos_system.entity.ShiftCashEvent;
import com.devcore.pos_system.entity.ShiftCashEventType;
import com.devcore.pos_system.entity.ShiftStatus;
import com.devcore.pos_system.modules.currency.application.CurrencyService;
import com.devcore.pos_system.modules.currency.domain.Currency;
import com.devcore.pos_system.repository.AppUserRepo;
import com.devcore.pos_system.repository.SaleRepo;
import com.devcore.pos_system.repository.ShiftCashEventRepo;
import com.devcore.pos_system.repository.ShiftRepo;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

@Service
@Transactional
public class ShiftService {
    private static final TypeReference<Map<String, BigDecimal>> MAP_TYPE = new TypeReference<>() {};

    private final ShiftRepo shiftRepo;
    private final SaleRepo saleRepo;
    private final ShiftCashEventRepo shiftCashEventRepo;
    private final CurrencyService currencyService;
    private final AppUserRepo appUserRepo;
    private final AuditEventService auditEventService;
    private final I18nService i18nService;
    private final ObjectMapper objectMapper = new ObjectMapper().findAndRegisterModules();
    private final BigDecimal varianceThreshold;

    /**
     * Executes the ShiftService operation.
     * <p>Return value: A fully initialized ShiftService instance.</p>
     *
     * @param shiftRepo Parameter of type {@code ShiftRepo} used by this operation.
     * @param saleRepo Parameter of type {@code SaleRepo} used by this operation.
     * @param shiftCashEventRepo Parameter of type {@code ShiftCashEventRepo} used by this operation.
     * @param currencyService Parameter of type {@code CurrencyService} used by this operation.
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * @param i18nService Parameter of type {@code I18nService} used by this operation.
     * @param varianceThreshold Parameter of type {@code BigDecimal} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ShiftService(ShiftRepo shiftRepo,
                        SaleRepo saleRepo,
                        ShiftCashEventRepo shiftCashEventRepo,
                        CurrencyService currencyService,
                        AppUserRepo appUserRepo,
                        AuditEventService auditEventService,
                        I18nService i18nService,
                        @Value("${app.shift.variance-threshold:20.00}") BigDecimal varianceThreshold) {
        this.shiftRepo = shiftRepo;
        this.saleRepo = saleRepo;
        this.shiftCashEventRepo = shiftCashEventRepo;
        this.currencyService = currencyService;
        this.appUserRepo = appUserRepo;
        this.auditEventService = auditEventService;
        this.i18nService = i18nService;
        this.varianceThreshold = varianceThreshold == null ? new BigDecimal("20.00") : varianceThreshold.abs();
    }

    /**
     * Executes the openShift operation.
     *
     * @param openedBy Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param openingFloatByCurrency Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @return {@code Shift} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Shift openShift(String openedBy, String terminalId, Map<String, BigDecimal> openingFloatByCurrency) {
        String actor = sanitize(openedBy);
        if (actor == null) {
            throw new IllegalStateException(msg("shift.error.signInOpen"));
        }
        String safeTerminalId = sanitize(terminalId);
        if (safeTerminalId == null) {
            throw new IllegalStateException(msg("shift.error.terminalRequiredOpen"));
        }
        if (findOpenShift(actor, safeTerminalId).isPresent()) {
            throw new IllegalStateException(msg("shift.error.alreadyOpen"));
        }
        Map<String, BigDecimal> opening = normalizeAmounts(openingFloatByCurrency);
        String baseCode = baseCode();
        opening.putIfAbsent(baseCode, BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));

        Shift shift = new Shift();
        shift.setCashierUsername(actor);
        shift.setOpenedBy(actor);
        shift.setOpenedByUserId(resolveActorUserId(actor));
        shift.setOpenedAt(LocalDateTime.now());
        shift.setStatus(ShiftStatus.OPEN);
        shift.setTerminalId(safeTerminalId);

        BigDecimal openingCashBase = opening.getOrDefault(baseCode, BigDecimal.ZERO);
        shift.setOpeningCash(openingCashBase);
        shift.setOpeningFloatJson(toJson(opening));
        shift.setCashInTotal(BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
        shift.setCashOutTotal(BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
        shift.setCashRefundTotal(BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
        shift.setExpectedCash(openingCashBase);
        shift.setVarianceCash(BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
        shift.setTotalSales(BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
        shift.setCashTotal(BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
        shift.setCardTotal(BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
        shift.setQrTotal(BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));

        Shift saved = shiftRepo.save(shift);
        auditEventService.record(
                "SHIFT_OPEN",
                "SHIFT",
                saved.getId(),
                null,
                shiftSnapshot(saved),
                Map.of("openingFloatByCurrency", opening, "terminalId", safeTerminalId)
        );
        return saved;
    }

    /**
     * Executes the addCashEvent operation.
     *
     * @param actorUsername Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param type Parameter of type {@code ShiftCashEventType} used by this operation.
     * @param currencyCode Parameter of type {@code String} used by this operation.
     * @param amount Parameter of type {@code BigDecimal} used by this operation.
     * @param reason Parameter of type {@code String} used by this operation.
     * @return {@code ShiftCashEvent} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ShiftCashEvent addCashEvent(String actorUsername,
                                       String terminalId,
                                       ShiftCashEventType type,
                                       String currencyCode,
                                       BigDecimal amount,
                                       String reason) {
        String actor = sanitize(actorUsername);
        if (actor == null) {
            throw new IllegalStateException(msg("shift.error.signInCashMovement"));
        }
        Shift shift = findOpenShift(actor, terminalId)
                .orElseThrow(() -> new IllegalStateException(msg("shift.error.noOpenShift")));
        if (type == null) {
            throw new IllegalStateException(msg("shift.error.selectCashEvent"));
        }
        boolean drawerOpen = type == ShiftCashEventType.DRAWER_OPEN;
        BigDecimal safeAmount = amount == null ? null : amount.max(BigDecimal.ZERO);
        if (!drawerOpen && (safeAmount == null || safeAmount.compareTo(BigDecimal.ZERO) <= 0)) {
            throw new IllegalStateException(msg("shift.error.amountPositive"));
        }
        String safeReason = trimTo(reason, 255);
        if (!drawerOpen && safeReason == null) {
            throw new IllegalStateException(msg("shift.error.reasonRequired"));
        }
        if (drawerOpen && safeReason == null) {
            safeReason = msg("shift.drawerOpened");
        }

        Currency currency = drawerOpen ? resolveCurrency(baseCode()) : resolveCurrency(currencyCode);
        BigDecimal rate = safeRate(currency);
        BigDecimal amountScaled = drawerOpen
                ? BigDecimal.ZERO.setScale(currencyDecimals(currency), RoundingMode.HALF_UP)
                : safeAmount.setScale(currencyDecimals(currency), RoundingMode.HALF_UP);
        BigDecimal baseAmount = drawerOpen
                ? BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP)
                : amountScaled.multiply(rate).setScale(2, RoundingMode.HALF_UP);

        String effectiveTerminal = sanitize(shift.getTerminalId()) != null ? sanitize(shift.getTerminalId()) : sanitize(terminalId);

        ShiftCashEvent event = new ShiftCashEvent();
        event.setShift(shift);
        event.setCreatedAt(LocalDateTime.now());
        event.setEventType(type);
        event.setCurrencyCode(currency.getCode());
        event.setAmount(amountScaled);
        event.setBaseAmount(baseAmount);
        event.setReason(safeReason);
        event.setActorUsername(actor);
        event.setActorUserId(appUserRepo.findByUsername(actor).map(u -> u.getId()).orElse(null));
        event.setTerminalId(effectiveTerminal);
        ShiftCashEvent saved = shiftCashEventRepo.save(event);

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("shiftId", shift.getId());
        metadata.put("currency", saved.getCurrencyCode());
        metadata.put("amount", saved.getAmount());
        metadata.put("baseAmount", saved.getBaseAmount());
        metadata.put("reason", saved.getReason());
        metadata.put("terminalId", saved.getTerminalId());
        String actionType = switch (type) {
            case CASH_IN -> "SHIFT_CASH_IN";
            case CASH_OUT -> "SHIFT_CASH_OUT";
            case DRAWER_OPEN -> "SHIFT_DRAWER_OPEN";
        };
        auditEventService.record(
                actionType,
                "SHIFT",
                shift.getId(),
                null,
                cashEventSnapshot(saved),
                metadata
        );
        return saved;
    }

    /**
     * Executes the closeShift operation.
     *
     * @param actorUsername Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param countedByCurrency Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @param managerAllowedForVariance Parameter of type {@code boolean} used by this operation.
     * @return {@code ShiftCloseResult} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ShiftCloseResult closeShift(String actorUsername,
                                       String terminalId,
                                       Map<String, BigDecimal> countedByCurrency,
                                       String notes,
                                       boolean managerAllowedForVariance) {
        String actor = sanitize(actorUsername);
        if (actor == null) {
            throw new IllegalStateException(msg("shift.error.signInClose"));
        }
        Shift shift = findOpenShift(actor, terminalId)
                .orElseThrow(() -> new IllegalStateException(msg("shift.error.noOpenShiftToClose")));

        String baseCode = baseCode();
        Map<String, BigDecimal> opening = parseAmounts(shift.getOpeningFloatJson());
        opening.putIfAbsent(baseCode, safeAmount(shift.getOpeningCash()));
        Map<String, BigDecimal> counted = normalizeAmounts(countedByCurrency);
        counted.putIfAbsent(baseCode, BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));

        ShiftReconciliationData reconciliation = buildReconciliation(shift, opening, counted);
        BigDecimal absVariance = reconciliation.varianceBase().abs();
        if (absVariance.compareTo(varianceThreshold) > 0 && !managerAllowedForVariance) {
            throw new IllegalStateException(msg("shift.error.varianceApproval", formatMoney(varianceThreshold)));
        }

        Map<String, Object> before = shiftSnapshot(shift);
        shift.setClosedAt(LocalDateTime.now());
        shift.setClosedBy(actor);
        shift.setClosedByUserId(resolveActorUserId(actor));
        shift.setStatus(ShiftStatus.CLOSED);
        shift.setCloseNotes(trimTo(notes, 1000));
        if (sanitize(shift.getTerminalId()) == null) {
            shift.setTerminalId(sanitize(terminalId));
        }

        shift.setTotalSales(reconciliation.totalSales());
        shift.setCashTotal(reconciliation.cashSalesBase());
        shift.setCardTotal(reconciliation.cardTotal());
        shift.setQrTotal(reconciliation.qrTotal());
        shift.setCashInTotal(reconciliation.cashInBase());
        shift.setCashOutTotal(reconciliation.cashOutBase());
        shift.setCashRefundTotal(reconciliation.cashRefundBase());
        shift.setExpectedCash(reconciliation.expectedBase());
        shift.setClosingCash(reconciliation.countedBase());
        shift.setVarianceCash(reconciliation.varianceBase());

        shift.setExpectedAmountsJson(toJson(reconciliation.expectedByCurrency()));
        shift.setCountedAmountsJson(toJson(reconciliation.countedByCurrency()));
        shift.setVarianceAmountsJson(toJson(reconciliation.varianceByCurrency()));

        Shift saved = shiftRepo.save(shift);
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("varianceThreshold", varianceThreshold);
        metadata.put("managerApproved", managerAllowedForVariance);
        metadata.put("terminalId", sanitize(saved.getTerminalId()) != null ? sanitize(saved.getTerminalId()) : sanitize(terminalId));
        metadata.put("cashEventCount", reconciliation.cashEventCount());
        metadata.put("cashRefundBase", reconciliation.cashRefundBase());
        auditEventService.record("SHIFT_CLOSE", "SHIFT", saved.getId(), before, shiftSnapshot(saved), metadata);

        return new ShiftCloseResult(saved, reconciliation);
    }

    /**
     * Executes the findOpenShift operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code Optional<Shift>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findOpenShift operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code Optional<Shift>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findOpenShift operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code Optional<Shift>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public Optional<Shift> findOpenShift(String username, String terminalId) {
        String actor = sanitize(username);
        if (actor == null) return Optional.empty();
        String terminal = sanitize(terminalId);
        if (terminal != null) {
            Optional<Shift> byTerminal = shiftRepo.findByCashierUsernameAndTerminalIdAndStatus(actor, terminal, ShiftStatus.OPEN);
            if (byTerminal.isPresent()) return byTerminal;
        }
        return shiftRepo.findByCashierUsernameAndStatus(actor, ShiftStatus.OPEN);
    }

    /**
     * Executes the listCashEvents operation.
     *
     * @param shiftId Parameter of type {@code Long} used by this operation.
     * @return {@code List<ShiftCashEvent>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the listCashEvents operation.
     *
     * @param shiftId Parameter of type {@code Long} used by this operation.
     * @return {@code List<ShiftCashEvent>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the listCashEvents operation.
     *
     * @param shiftId Parameter of type {@code Long} used by this operation.
     * @return {@code List<ShiftCashEvent>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public List<ShiftCashEvent> listCashEvents(Long shiftId) {
        if (shiftId == null) return List.of();
        return shiftCashEventRepo.findByShift_IdOrderByCreatedAtAsc(shiftId);
    }

    /**
     * Executes the parseAmounts operation.
     *
     * @param json Parameter of type {@code String} used by this operation.
     * @return {@code Map<String, BigDecimal>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the parseAmounts operation.
     *
     * @param json Parameter of type {@code String} used by this operation.
     * @return {@code Map<String, BigDecimal>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the parseAmounts operation.
     *
     * @param json Parameter of type {@code String} used by this operation.
     * @return {@code Map<String, BigDecimal>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public Map<String, BigDecimal> parseAmounts(String json) {
        if (json == null || json.isBlank()) return new LinkedHashMap<>();
        try {
            Map<String, BigDecimal> parsed = objectMapper.readValue(json, MAP_TYPE);
            return normalizeAmounts(parsed);
        } catch (Exception ex) {
            return new LinkedHashMap<>();
        }
    }

    /**
     * Executes the previewReconciliation operation.
     *
     * @param shift Parameter of type {@code Shift} used by this operation.
     * @param countedByCurrency Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @return {@code ShiftReconciliationData} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the previewReconciliation operation.
     *
     * @param shift Parameter of type {@code Shift} used by this operation.
     * @param countedByCurrency Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @return {@code ShiftReconciliationData} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the previewReconciliation operation.
     *
     * @param shift Parameter of type {@code Shift} used by this operation.
     * @param countedByCurrency Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @return {@code ShiftReconciliationData} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public ShiftReconciliationData previewReconciliation(Shift shift, Map<String, BigDecimal> countedByCurrency) {
        if (shift == null) {
            return ShiftReconciliationData.empty();
        }
        String baseCode = baseCode();
        Map<String, BigDecimal> opening = parseAmounts(shift.getOpeningFloatJson());
        opening.putIfAbsent(baseCode, safeAmount(shift.getOpeningCash()));
        Map<String, BigDecimal> counted = normalizeAmounts(countedByCurrency);
        counted.putIfAbsent(baseCode, BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
        return buildReconciliation(shift, opening, counted);
    }

    /**
     * Executes the varianceThreshold operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal varianceThreshold() {
        return varianceThreshold;
    }

    /**
     * Executes the buildReconciliation operation.
     *
     * @param shift Parameter of type {@code Shift} used by this operation.
     * @param openingByCurrency Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @param countedByCurrency Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @return {@code ShiftReconciliationData} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private ShiftReconciliationData buildReconciliation(Shift shift,
                                                        Map<String, BigDecimal> openingByCurrency,
                                                        Map<String, BigDecimal> countedByCurrency) {
        String baseCode = baseCode();
        List<Sale> sales = saleRepo.findByShift_Id(shift.getId());
        Map<String, BigDecimal> cashSalesByCurrency = new LinkedHashMap<>();
        Map<String, BigDecimal> cashRefundsByCurrency = new LinkedHashMap<>();
        BigDecimal cashSalesBase = BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal cashRefundBase = BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal cardTotal = BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal qrTotal = BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal totalSales = BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);

        for (Sale sale : sales) {
            if (sale.getStatus() == SaleStatus.VOID) continue;
            BigDecimal net = safeNetTotal(sale);
            totalSales = totalSales.add(net);
            BigDecimal saleRefund = safeCashRefund(sale);
            if (saleRefund.compareTo(BigDecimal.ZERO) > 0) {
                cashRefundBase = cashRefundBase.add(saleRefund);
                addAmount(cashRefundsByCurrency, baseCode, saleRefund);
            }

            if (sale.getPayments() != null && !sale.getPayments().isEmpty()) {
                for (SalePayment payment : sale.getPayments()) {
                    if (payment == null || payment.getMethod() == null) continue;
                    BigDecimal baseAmount = safeAmount(payment.getAmount());
                    if (payment.getMethod() == PaymentMethod.CASH) {
                        String code = normalizeCode(payment.getCurrencyCode(), baseCode);
                        BigDecimal foreign = payment.getForeignAmount();
                        BigDecimal currencyAmount = (foreign != null ? foreign : baseAmount).setScale(4, RoundingMode.HALF_UP);
                        addAmount(cashSalesByCurrency, code, currencyAmount);
                        cashSalesBase = cashSalesBase.add(baseAmount);
                    } else if (payment.getMethod() == PaymentMethod.CARD) {
                        cardTotal = cardTotal.add(baseAmount);
                    } else if (payment.getMethod() == PaymentMethod.QR) {
                        qrTotal = qrTotal.add(baseAmount);
                    }
                }
            } else if (sale.getPaymentMethod() != null) {
                if (sale.getPaymentMethod() == PaymentMethod.CASH) {
                    addAmount(cashSalesByCurrency, baseCode, net);
                    cashSalesBase = cashSalesBase.add(net);
                } else if (sale.getPaymentMethod() == PaymentMethod.CARD) {
                    cardTotal = cardTotal.add(net);
                } else if (sale.getPaymentMethod() == PaymentMethod.QR) {
                    qrTotal = qrTotal.add(net);
                }
            }
        }

        List<ShiftCashEvent> cashEvents = shiftCashEventRepo.findByShift_IdOrderByCreatedAtAsc(shift.getId());
        Map<String, BigDecimal> adjustmentsByCurrency = new LinkedHashMap<>();
        BigDecimal cashInBase = BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        BigDecimal cashOutBase = BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        for (ShiftCashEvent event : cashEvents) {
            if (event == null) continue;
            String code = normalizeCode(event.getCurrencyCode(), baseCode);
            BigDecimal eventAmount = safeAmount(event.getAmount()).setScale(4, RoundingMode.HALF_UP);
            BigDecimal baseAmount = safeAmount(event.getBaseAmount());
            if (event.getEventType() == ShiftCashEventType.CASH_OUT) {
                addAmount(adjustmentsByCurrency, code, eventAmount.negate());
                cashOutBase = cashOutBase.add(baseAmount);
            } else if (event.getEventType() == ShiftCashEventType.CASH_IN) {
                addAmount(adjustmentsByCurrency, code, eventAmount);
                cashInBase = cashInBase.add(baseAmount);
            }
        }

        Map<String, BigDecimal> expectedByCurrency = new LinkedHashMap<>();
        merge(expectedByCurrency, openingByCurrency);
        merge(expectedByCurrency, cashSalesByCurrency);
        merge(expectedByCurrency, adjustmentsByCurrency);
        subtract(expectedByCurrency, cashRefundsByCurrency);

        Set<String> currencies = unionKeys(expectedByCurrency, countedByCurrency);
        Map<String, BigDecimal> varianceByCurrency = new LinkedHashMap<>();
        for (String code : currencies) {
            BigDecimal expected = expectedByCurrency.getOrDefault(code, BigDecimal.ZERO);
            BigDecimal counted = countedByCurrency.getOrDefault(code, BigDecimal.ZERO);
            varianceByCurrency.put(code, counted.subtract(expected).setScale(4, RoundingMode.HALF_UP));
        }

        BigDecimal expectedBase = toBaseTotal(expectedByCurrency);
        BigDecimal countedBase = toBaseTotal(countedByCurrency);
        BigDecimal varianceBase = countedBase.subtract(expectedBase).setScale(2, RoundingMode.HALF_UP);

        return new ShiftReconciliationData(
                normalizeAmounts(expectedByCurrency),
                normalizeAmounts(countedByCurrency),
                normalizeAmounts(varianceByCurrency),
                expectedBase,
                countedBase,
                varianceBase,
                totalSales.setScale(2, RoundingMode.HALF_UP),
                cashSalesBase.setScale(2, RoundingMode.HALF_UP),
                cashRefundBase.setScale(2, RoundingMode.HALF_UP),
                cardTotal.setScale(2, RoundingMode.HALF_UP),
                qrTotal.setScale(2, RoundingMode.HALF_UP),
                cashInBase.setScale(2, RoundingMode.HALF_UP),
                cashOutBase.setScale(2, RoundingMode.HALF_UP),
                cashEvents.size()
        );
    }

    /**
     * Executes the toBaseTotal operation.
     *
     * @param amountsByCurrency Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal toBaseTotal(Map<String, BigDecimal> amountsByCurrency) {
        BigDecimal total = BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        for (Map.Entry<String, BigDecimal> entry : amountsByCurrency.entrySet()) {
            if (entry.getValue() == null) continue;
            Currency currency = resolveCurrency(entry.getKey());
            BigDecimal rate = safeRate(currency);
            total = total.add(entry.getValue().multiply(rate));
        }
        return total.setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Executes the unionKeys operation.
     *
     * @param first Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @param second Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @return {@code Set<String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Set<String> unionKeys(Map<String, BigDecimal> first, Map<String, BigDecimal> second) {
        Set<String> keys = new LinkedHashSet<>();
        if (first != null) keys.addAll(first.keySet());
        if (second != null) keys.addAll(second.keySet());
        return keys;
    }

    /**
     * Executes the merge operation.
     *
     * @param target Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @param source Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void merge(Map<String, BigDecimal> target, Map<String, BigDecimal> source) {
        if (source == null) return;
        for (Map.Entry<String, BigDecimal> entry : source.entrySet()) {
            addAmount(target, entry.getKey(), entry.getValue());
        }
    }

    /**
     * Executes the subtract operation.
     *
     * @param target Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @param source Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void subtract(Map<String, BigDecimal> target, Map<String, BigDecimal> source) {
        if (source == null) return;
        for (Map.Entry<String, BigDecimal> entry : source.entrySet()) {
            BigDecimal value = entry.getValue();
            if (value == null) continue;
            addAmount(target, entry.getKey(), value.negate());
        }
    }

    /**
     * Executes the addAmount operation.
     *
     * @param target Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @param currencyCode Parameter of type {@code String} used by this operation.
     * @param amount Parameter of type {@code BigDecimal} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void addAmount(Map<String, BigDecimal> target, String currencyCode, BigDecimal amount) {
        if (target == null || amount == null) return;
        String code = normalizeCode(currencyCode, baseCode());
        BigDecimal current = target.getOrDefault(code, BigDecimal.ZERO);
        target.put(code, current.add(amount).setScale(4, RoundingMode.HALF_UP));
    }

    /**
     * Executes the formatMoney operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String formatMoney(BigDecimal value) {
        BigDecimal safe = safeAmount(value).setScale(2, RoundingMode.HALF_UP);
        Currency base = currencyService.getBaseCurrency();
        String symbol = base != null ? base.getSymbol() : "$";
        return (symbol == null ? "" : symbol) + safe.toPlainString();
    }

    /**
     * Executes the resolveCurrency operation.
     *
     * @param code Parameter of type {@code String} used by this operation.
     * @return {@code Currency} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Currency resolveCurrency(String code) {
        Currency base = currencyService.getBaseCurrency();
        String normalized = normalizeCode(code, base == null ? "USD" : base.getCode());
        Currency found = currencyService.findByCode(normalized);
        if (found != null && Boolean.TRUE.equals(found.getActive())) {
            return found;
        }
        return base;
    }

    /**
     * Executes the currencyDecimals operation.
     *
     * @param currency Parameter of type {@code Currency} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int currencyDecimals(Currency currency) {
        if (currency == null || currency.getFractionDigits() == null) return 2;
        return Math.max(0, currency.getFractionDigits());
    }

    /**
     * Executes the safeRate operation.
     *
     * @param currency Parameter of type {@code Currency} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeRate(Currency currency) {
        if (currency == null || currency.getRateToBase() == null || currency.getRateToBase().compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ONE;
        }
        return currency.getRateToBase();
    }

    /**
     * Executes the baseCode operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String baseCode() {
        Currency base = currencyService.getBaseCurrency();
        return base == null || base.getCode() == null ? "USD" : base.getCode().toUpperCase();
    }

    /**
     * Executes the normalizeCode operation.
     *
     * @param code Parameter of type {@code String} used by this operation.
     * @param fallback Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalizeCode(String code, String fallback) {
        String normalized = sanitize(code);
        if (normalized == null) return fallback == null ? "USD" : fallback.toUpperCase();
        return normalized.toUpperCase();
    }

    /**
     * Executes the normalizeAmounts operation.
     *
     * @param raw Parameter of type {@code Map<String, BigDecimal>} used by this operation.
     * @return {@code Map<String, BigDecimal>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, BigDecimal> normalizeAmounts(Map<String, BigDecimal> raw) {
        Map<String, BigDecimal> normalized = new LinkedHashMap<>();
        if (raw == null) return normalized;
        for (Map.Entry<String, BigDecimal> entry : raw.entrySet()) {
            String code = normalizeCode(entry.getKey(), null);
            if (code == null) continue;
            BigDecimal value = safeAmount(entry.getValue()).setScale(4, RoundingMode.HALF_UP);
            normalized.put(code, value);
        }
        return normalized;
    }

    /**
     * Executes the safeAmount operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeAmount(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    /**
     * Executes the safeCashRefund operation.
     *
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeCashRefund(Sale sale) {
        if (sale == null) {
            return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        }
        BigDecimal refunded = safeAmount(sale.getRefundedTotal());
        BigDecimal total = safeAmount(sale.getTotal());
        if (refunded.compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        }
        if (refunded.compareTo(total) > 0) {
            refunded = total;
        }
        return refunded.setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Executes the resolveActorUserId operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @return {@code Long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Long resolveActorUserId(String username) {
        String actor = sanitize(username);
        if (actor == null) return null;
        return appUserRepo.findByUsername(actor).map(u -> u.getId()).orElse(null);
    }

    /**
     * Executes the safeNetTotal operation.
     *
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeNetTotal(Sale sale) {
        BigDecimal total = safeAmount(sale.getTotal());
        BigDecimal refunded = safeAmount(sale.getRefundedTotal());
        BigDecimal net = total.subtract(refunded);
        if (net.compareTo(BigDecimal.ZERO) < 0) {
            return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        }
        return net.setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Executes the toJson operation.
     *
     * @param value Parameter of type {@code Object} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String toJson(Object value) {
        try {
            return objectMapper.writeValueAsString(value);
        } catch (Exception ex) {
            throw new IllegalStateException(msg("shift.error.serialize"), ex);
        }
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
    private String msg(String key, Object... args) {
        return i18nService.msg(key, args);
    }

    /**
     * Executes the sanitize operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String sanitize(String value) {
        if (value == null) return null;
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    /**
     * Executes the trimTo operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @param maxLength Parameter of type {@code int} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String trimTo(String value, int maxLength) {
        String safe = sanitize(value);
        if (safe == null) return null;
        if (safe.length() <= maxLength) return safe;
        return safe.substring(0, maxLength);
    }

    /**
     * Executes the shiftSnapshot operation.
     *
     * @param shift Parameter of type {@code Shift} used by this operation.
     * @return {@code Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, Object> shiftSnapshot(Shift shift) {
        if (shift == null) return null;
        Map<String, Object> snapshot = new LinkedHashMap<>();
        snapshot.put("id", shift.getId());
        snapshot.put("status", shift.getStatus() == null ? null : shift.getStatus().name());
        snapshot.put("cashierUsername", shift.getCashierUsername());
        snapshot.put("openedBy", shift.getOpenedBy());
        snapshot.put("openedByUserId", shift.getOpenedByUserId());
        snapshot.put("openedAt", shift.getOpenedAt());
        snapshot.put("closedBy", shift.getClosedBy());
        snapshot.put("closedByUserId", shift.getClosedByUserId());
        snapshot.put("closedAt", shift.getClosedAt());
        snapshot.put("openingCash", shift.getOpeningCash());
        snapshot.put("closingCash", shift.getClosingCash());
        snapshot.put("expectedCash", shift.getExpectedCash());
        snapshot.put("varianceCash", shift.getVarianceCash());
        snapshot.put("totalSales", shift.getTotalSales());
        snapshot.put("cashTotal", shift.getCashTotal());
        snapshot.put("cardTotal", shift.getCardTotal());
        snapshot.put("qrTotal", shift.getQrTotal());
        snapshot.put("cashInTotal", shift.getCashInTotal());
        snapshot.put("cashOutTotal", shift.getCashOutTotal());
        snapshot.put("cashRefundTotal", shift.getCashRefundTotal());
        snapshot.put("terminalId", shift.getTerminalId());
        snapshot.put("openingFloatJson", shift.getOpeningFloatJson());
        snapshot.put("countedAmountsJson", shift.getCountedAmountsJson());
        snapshot.put("expectedAmountsJson", shift.getExpectedAmountsJson());
        snapshot.put("varianceAmountsJson", shift.getVarianceAmountsJson());
        snapshot.put("closeNotes", shift.getCloseNotes());
        return snapshot;
    }

    /**
     * Executes the cashEventSnapshot operation.
     *
     * @param event Parameter of type {@code ShiftCashEvent} used by this operation.
     * @return {@code Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Map<String, Object> cashEventSnapshot(ShiftCashEvent event) {
        if (event == null) return null;
        Map<String, Object> snapshot = new LinkedHashMap<>();
        snapshot.put("id", event.getId());
        snapshot.put("shiftId", event.getShift() == null ? null : event.getShift().getId());
        snapshot.put("createdAt", event.getCreatedAt());
        snapshot.put("eventType", event.getEventType() == null ? null : event.getEventType().name());
        snapshot.put("currencyCode", event.getCurrencyCode());
        snapshot.put("amount", event.getAmount());
        snapshot.put("baseAmount", event.getBaseAmount());
        snapshot.put("reason", event.getReason());
        snapshot.put("actorUsername", event.getActorUsername());
        snapshot.put("actorUserId", event.getActorUserId());
        snapshot.put("terminalId", event.getTerminalId());
        return snapshot;
    }

    public record ShiftCloseResult(Shift shift, ShiftReconciliationData reconciliation) {}

    public record ShiftReconciliationData(
            Map<String, BigDecimal> expectedByCurrency,
            Map<String, BigDecimal> countedByCurrency,
            Map<String, BigDecimal> varianceByCurrency,
            BigDecimal expectedBase,
            BigDecimal countedBase,
            BigDecimal varianceBase,
            BigDecimal totalSales,
            BigDecimal cashSalesBase,
            BigDecimal cashRefundBase,
            BigDecimal cardTotal,
            BigDecimal qrTotal,
            BigDecimal cashInBase,
            BigDecimal cashOutBase,
            int cashEventCount
    ) {
        /**
         * Executes the empty operation.
         *
         * @return {@code ShiftReconciliationData} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        public static ShiftReconciliationData empty() {
            return new ShiftReconciliationData(
                    Map.of(),
                    Map.of(),
                    Map.of(),
                    BigDecimal.ZERO,
                    BigDecimal.ZERO,
                    BigDecimal.ZERO,
                    BigDecimal.ZERO,
                    BigDecimal.ZERO,
                    BigDecimal.ZERO,
                    BigDecimal.ZERO,
                    BigDecimal.ZERO,
                    BigDecimal.ZERO,
                    BigDecimal.ZERO,
                    0
            );
        }
    }
}
