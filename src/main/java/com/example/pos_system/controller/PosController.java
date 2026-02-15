package com.example.pos_system.controller;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.dto.CartItem;
import com.example.pos_system.entity.*;
import com.example.pos_system.repository.*;
import com.example.pos_system.service.CheckoutAttemptService;
import com.example.pos_system.service.CurrencyService;
import com.example.pos_system.service.PosCartService;
import com.example.pos_system.service.PosHardwareService;
import com.example.pos_system.service.ProductFeedService;
import com.example.pos_system.service.PosService;
import com.example.pos_system.service.EndpointRateLimiterService;
import com.example.pos_system.service.PaginationObservabilityService;
import com.example.pos_system.service.ShiftService;
import com.example.pos_system.service.TerminalSettingsService;
import com.example.pos_system.service.I18nService;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.bind.support.SessionStatus;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Controller
@RequestMapping("/pos")
@SessionAttributes("cart")
public class PosController {
  private static final Logger log = LoggerFactory.getLogger(PosController.class);
  private final ProductRepo productRepo;
  private final CategoryRepo categoryRepo;
  private final PosService posService;
  private final HeldSaleRepo heldSaleRepo;
  private final CustomerRepo customerRepo;
  private final ShiftService shiftService;
  private final CurrencyService currencyService;
  private final PosCartService posCartService;
  private final CheckoutAttemptService checkoutAttemptService;
  private final TerminalSettingsService terminalSettingsService;
  private final PosHardwareService posHardwareService;
  private final ProductFeedService productFeedService;
  private final EndpointRateLimiterService endpointRateLimiterService;
  private final PaginationObservabilityService paginationObservabilityService;
  private final I18nService i18nService;

  @Value("${app.pagination.feed.rate-limit-per-minute:240}")
  private int productFeedRateLimitPerMinute;

  public PosController(ProductRepo productRepo,
                       CategoryRepo categoryRepo,
                       PosService posService,
                       HeldSaleRepo heldSaleRepo,
                       CustomerRepo customerRepo,
                       ShiftService shiftService,
                       CurrencyService currencyService,
                       PosCartService posCartService,
                       CheckoutAttemptService checkoutAttemptService,
                       TerminalSettingsService terminalSettingsService,
                       PosHardwareService posHardwareService,
                       ProductFeedService productFeedService,
                       EndpointRateLimiterService endpointRateLimiterService,
                       PaginationObservabilityService paginationObservabilityService,
                       I18nService i18nService) {
    this.productRepo = productRepo;
    this.categoryRepo = categoryRepo;
    this.posService = posService;
    this.heldSaleRepo = heldSaleRepo;
    this.customerRepo = customerRepo;
    this.shiftService = shiftService;
    this.currencyService = currencyService;
    this.posCartService = posCartService;
    this.checkoutAttemptService = checkoutAttemptService;
    this.terminalSettingsService = terminalSettingsService;
    this.posHardwareService = posHardwareService;
    this.productFeedService = productFeedService;
    this.endpointRateLimiterService = endpointRateLimiterService;
    this.paginationObservabilityService = paginationObservabilityService;
    this.i18nService = i18nService;
  }

  @ModelAttribute("cart")
  public Cart cart() { return new Cart(); }

  @GetMapping
  public String pos(@RequestParam(required=false) String q,
                    @RequestParam(required=false) Long categoryId,
                    @RequestParam(required=false) Integer page,
                    @RequestParam(required=false) String scanError,
                    @RequestParam(required=false) String cartError,
                    @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                    @ModelAttribute("cart") Cart cart,
                    Model model) {
    model.addAttribute("categories", categoryRepo.findAll(Sort.by("sortOrder").ascending().and(Sort.by("name").ascending())));
    ProductFeedService.ProductFeedSlice initialFeed = productFeedService.fetchFeed(q, categoryId, null, 24);
    model.addAttribute("products", initialFeed.items());
    model.addAttribute("hasMore", initialFeed.hasMore());
    model.addAttribute("nextCursor", initialFeed.nextCursor());
    model.addAttribute("q", initialFeed.normalizedQuery() == null ? "" : initialFeed.normalizedQuery());
    model.addAttribute("categoryId", categoryId);
    if (scanError != null && !scanError.isBlank()) {
      model.addAttribute("scanError", scanError);
    }
    if (cartError != null && !cartError.isBlank()) {
      model.addAttribute("cartError", cartError);
    }
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: productGridWrap" : "pos/index";
  }

  @GetMapping("/products")
  public String productsFragment(@RequestParam(required=false) String q,
                                 @RequestParam(required=false) Long categoryId,
                                 @RequestParam(defaultValue = "0") int page,
                                 @RequestParam(required = false) Boolean append,
                                 Model model) {
    ProductFeedService.ProductFeedSlice firstFeed = productFeedService.fetchFeed(q, categoryId, null, 24);
    model.addAttribute("products", firstFeed.items());
    model.addAttribute("hasMore", firstFeed.hasMore());
    model.addAttribute("nextCursor", firstFeed.nextCursor());
    model.addAttribute("q", firstFeed.normalizedQuery() == null ? "" : firstFeed.normalizedQuery());
    model.addAttribute("categoryId", categoryId);
    if (Boolean.TRUE.equals(append)) {
      return "pos/fragments :: productGridItems";
    }
    return "pos/fragments :: productGridWrap";
  }

  @GetMapping(value = "/products/feed", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public ProductFeedResponse productsFeed(@RequestParam(required = false) String q,
                                          @RequestParam(required = false) Long categoryId,
                                          @RequestParam(required = false) String cursor,
                                          @RequestParam(required = false, defaultValue = "24") Integer size,
                                          HttpServletRequest request) {
    long startedAt = System.nanoTime();
    String actor = currentUsername();
    String sessionId = request == null || request.getSession(false) == null ? "anon-session" : request.getSession(false).getId();
    String remoteIp = request == null ? "unknown" : request.getRemoteAddr();
    String rateLimitKey = (actor == null ? "anon" : actor) + "|" + sessionId + "|" + remoteIp;

    if (!endpointRateLimiterService.allow(rateLimitKey, productFeedRateLimitPerMinute, Duration.ofMinutes(1))) {
      long responseMs = nanosToMillis(System.nanoTime() - startedAt);
      PaginationObservabilityService.Snapshot snapshot = paginationObservabilityService.recordError(responseMs);
      log.warn("event=pos_product_feed_rate_limited user={} session={} ip={} q={} categoryId={} cursorPresent={} size={} latencyMs={} p95Ms={} errorRate={}",
              actor,
              sessionId,
              remoteIp,
              q,
              categoryId,
              cursor != null && !cursor.isBlank(),
              size,
              responseMs,
              snapshot.p95ResponseMs(),
              snapshot.errorRate());
      throw new ResponseStatusException(HttpStatus.TOO_MANY_REQUESTS, msg("pos.error.rateLimitedPagination"));
    }

    long dbStartedAt = System.nanoTime();
    try {
      ProductFeedService.ProductFeedSlice slice = productFeedService.fetchFeed(q, categoryId, cursor, size);
      long dbMs = nanosToMillis(System.nanoTime() - dbStartedAt);
      long responseMs = nanosToMillis(System.nanoTime() - startedAt);
      PaginationObservabilityService.Snapshot snapshot = paginationObservabilityService.recordSuccess(responseMs, dbMs);
      List<ProductFeedService.ProductFeedItem> items = slice.items().stream()
              .map(productFeedService::toFeedItem)
              .toList();

      log.info("event=pos_product_feed user={} session={} ip={} q={} categoryId={} cursorPresent={} requestedSize={} returned={} hasMore={} latencyMs={} dbMs={} p95Ms={} dbP95Ms={} errorRate={}",
              actor,
              sessionId,
              remoteIp,
              slice.normalizedQuery(),
              slice.categoryId(),
              cursor != null && !cursor.isBlank(),
              size,
              items.size(),
              slice.hasMore(),
              responseMs,
              dbMs,
              snapshot.p95ResponseMs(),
              snapshot.p95DbMs(),
              snapshot.errorRate());

      return new ProductFeedResponse(items, slice.nextCursor(), slice.hasMore(), null);
    } catch (IllegalArgumentException ex) {
      long responseMs = nanosToMillis(System.nanoTime() - startedAt);
      PaginationObservabilityService.Snapshot snapshot = paginationObservabilityService.recordError(responseMs);
      log.warn("event=pos_product_feed_bad_cursor user={} session={} ip={} q={} categoryId={} cursorPresent={} size={} latencyMs={} p95Ms={} errorRate={} message={}",
              actor,
              sessionId,
              remoteIp,
              q,
              categoryId,
              cursor != null && !cursor.isBlank(),
              size,
              responseMs,
              snapshot.p95ResponseMs(),
              snapshot.errorRate(),
              ex.getMessage());
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ex.getMessage());
    } catch (RuntimeException ex) {
      long responseMs = nanosToMillis(System.nanoTime() - startedAt);
      PaginationObservabilityService.Snapshot snapshot = paginationObservabilityService.recordError(responseMs);
      log.error("event=pos_product_feed_error user={} session={} ip={} q={} categoryId={} cursorPresent={} size={} latencyMs={} p95Ms={} errorRate={}",
              actor,
              sessionId,
              remoteIp,
              q,
              categoryId,
              cursor != null && !cursor.isBlank(),
              size,
              responseMs,
              snapshot.p95ResponseMs(),
              snapshot.errorRate(),
              ex);
      throw ex;
    }
  }

  @PostMapping("/cart/add/{productId}")
  public String addToCart(@PathVariable Long productId,
                          @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                          @ModelAttribute("cart") Cart cart,
                          Model model) {
    log.info("POS addToCart POST productId={} hxRequest={}", productId, hxRequest);
    Product p = productRepo.findById(productId).orElse(null);
    String error = addProductToCart(p, cart);
    if (error != null) {
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", error);
        enrichCartModel(model, cart);
        return "pos/fragments :: cartPanel";
      }
      return redirectWithCartError(error);
    }
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/quick-add")
  public String quickAdd(@RequestParam(required = false) String q,
                         @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                         @ModelAttribute("cart") Cart cart,
                         Model model) {
    if (q == null || q.isBlank()) {
      model.addAttribute("cartError", msg("pos.error.enterSkuOrBarcode"));
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : redirectWithCartError(msg("pos.error.enterSkuOrBarcode"));
    }
    String value = q.trim();
    Product p = productRepo.findByBarcode(value).orElse(null);
    if (p == null) {
      p = productRepo.findBySkuIgnoreCase(value).orElse(null);
    }
    String error = addProductToCart(p, cart);
    if (error != null) {
      model.addAttribute("cartError", error);
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : redirectWithCartError(error);
    }
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  // Fallback for clients accidentally issuing GETs (e.g., scanner apps or bad redirects)
  @GetMapping("/cart/add/{productId}")
  public String addToCartGet(@PathVariable Long productId,
                             @ModelAttribute("cart") Cart cart,
                             Model model) {
    log.info("POS addToCart GET productId={}", productId);
    return addToCart(productId, null, cart, model);
  }

  @PostMapping("/cart/update")
  public String updateQty(@RequestParam Long productId, @RequestParam int qty,
                          @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                          @ModelAttribute("cart") Cart cart, Model model) {
    log.info("POS updateQty POST productId={} qty={} hxRequest={}", productId, qty, hxRequest);
    cart.setQty(productId, qty);
    if (qty > 0) {
      Product p = productRepo.findById(productId).orElse(null);
      Customer customer = loadCustomer(cart.getCustomerId());
      applyAutoPricing(cart, p, customer);
    }
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/cart/unit")
  public String updateUnit(@RequestParam Long productId,
                           @RequestParam UnitType unitType,
                           @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                           @ModelAttribute("cart") Cart cart, Model model) {
    log.info("POS updateUnit POST productId={} unitType={} hxRequest={}", productId, unitType, hxRequest);
    Product p = productRepo.findById(productId).orElse(null);
    CartItem item = cart.getItem(productId);
    if (item == null) {
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
    }
    if (p == null) {
      String error = msg("pos.error.productNotFound");
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", error);
        enrichCartModel(model, cart);
        return "pos/fragments :: cartPanel";
      }
      return redirectWithCartError(error);
    }
    if (unitType == null) unitType = UnitType.PIECE;
    Integer unitSize = resolveUnitSizeForSelection(p, unitType);
    if (unitSize == null) {
      String error = unitType == UnitType.BOX ? msg("pos.error.boxSizeNotSet") : msg("pos.error.caseSizeNotSet");
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", error);
        enrichCartModel(model, cart);
        return "pos/fragments :: cartPanel";
      }
      return redirectWithCartError(error);
    }
    Customer customer = loadCustomer(cart.getCustomerId());
    CartItem beforeItem = copyCartItem(item);
    PriceTier priceTier = autoPriceTier(p, customer, item.getQty(), unitSize);
    String error = validateSaleable(p, priceTier, unitType);
    if (error != null) {
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", error);
        enrichCartModel(model, cart);
        return "pos/fragments :: cartPanel";
      }
      return redirectWithCartError(error);
    }
    cart.setUnit(productId, unitType, unitSize);
    BigDecimal unitPrice = resolveUnitPrice(p, priceTier, unitSize);
    cart.setPriceTier(productId, priceTier, unitPrice);
    CartItem afterItem = cart.getItem(productId);
    posCartService.recordPriceOverride(cart, beforeItem, afterItem, "unit-change");
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/cart/note")
  public String updateNote(@RequestParam Long productId,
                           @RequestParam(required = false) String note,
                           @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                           @ModelAttribute("cart") Cart cart, Model model) {
    cart.setNote(productId, note);
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/cart/customer/lookup")
  public String attachCustomer(@RequestParam(required = false) String query,
                               @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                               @ModelAttribute("cart") Cart cart,
                               Model model) {
    if (query == null || query.isBlank()) {
      model.addAttribute("cartError", msg("pos.error.enterPhoneOrEmail"));
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : redirectWithCartError(msg("pos.error.enterPhoneOrEmail"));
    }
    String value = query.trim();
    Customer customer = customerRepo.findByPhone(value)
            .orElseGet(() -> customerRepo.findByEmail(value).orElse(null));
    if (customer == null) {
      model.addAttribute("cartError", msg("pos.error.customerNotFound"));
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : redirectWithCartError(msg("pos.error.customerNotFound"));
    }
    cart.setCustomerId(customer.getId());
    applyAutoPricing(cart, customer);
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/cart/customer/create")
  public String createCustomer(@RequestParam String name,
                               @RequestParam(required = false) String phone,
                               @RequestParam(required = false) String email,
                               @RequestParam(required = false) Boolean wholesale,
                               @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                               @ModelAttribute("cart") Cart cart,
                               Model model) {
    if (name == null || name.isBlank()) {
      model.addAttribute("cartError", msg("pos.error.customerNameRequired"));
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : redirectWithCartError(msg("pos.error.customerNameRequired"));
    }
    Customer customer = new Customer();
    customer.setName(name.trim());
    if (phone != null && !phone.isBlank()) customer.setPhone(phone.trim());
    if (email != null && !email.isBlank()) customer.setEmail(email.trim());
    customer.setWholesale(Boolean.TRUE.equals(wholesale));
    try {
      customer = customerRepo.save(customer);
    } catch (DataIntegrityViolationException ex) {
      model.addAttribute("cartError", msg("pos.error.phoneEmailExists"));
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : redirectWithCartError(msg("pos.error.phoneEmailExists"));
    }
    cart.setCustomerId(customer.getId());
    applyAutoPricing(cart, customer);
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/cart/customer/clear")
  public String clearCustomer(@RequestHeader(value = "HX-Request", required = false) String hxRequest,
                              @ModelAttribute("cart") Cart cart,
                              Model model) {
    cart.setCustomerId(null);
    applyAutoPricing(cart, null);
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/hold")
  public String holdCart(@RequestParam(required = false) String label,
                         @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                         @ModelAttribute("cart") Cart cart,
                         Model model) {
    if (cart.getItems().isEmpty()) {
      model.addAttribute("cartError", msg("pos.error.cartEmpty"));
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : redirectWithCartError(msg("pos.error.cartEmpty"));
    }
    HeldSale hold = new HeldSale();
    hold.setCashierUsername(currentUsername());
    hold.setCreatedAt(LocalDateTime.now());
    hold.setLabel(label == null || label.isBlank() ? msg("pos.heldSaleDefaultLabel") : label.trim());
    hold.setDiscount(cart.getDiscount());
    hold.setDiscountType(cart.getDiscountType());
    hold.setDiscountValue(cart.getDiscountValue());
    hold.setDiscountReason(cart.getDiscountReason());
    hold.setTaxRate(cart.getTaxRate());
    Customer customer = loadCustomer(cart.getCustomerId());
    hold.setCustomer(customer);
    for (var ci : cart.getItems()) {
      HeldSaleItem item = new HeldSaleItem();
      item.setHeldSale(hold);
      item.setProduct(productRepo.findById(ci.getProductId()).orElse(null));
      item.setProductId(ci.getProductId());
      item.setName(ci.getName());
      item.setUnitPrice(ci.getUnitPrice());
      item.setQty(ci.getQty());
      item.setPriceTier(ci.getPriceTier());
      item.setUnitType(ci.getUnitType());
      item.setUnitSize(ci.getUnitSize());
      item.setNote(ci.getNote());
      hold.getItems().add(item);
    }
    HeldSale savedHold = heldSaleRepo.save(hold);
    posCartService.recordHoldCart(cart, savedHold);
    cart.clear();
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/holds/{id}/resume")
  public String resumeHold(@PathVariable Long id,
                           @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                           @ModelAttribute("cart") Cart cart,
                           Model model) {
    HeldSale hold = heldSaleRepo.findById(id).orElse(null);
    if (hold == null || (currentUsername() != null && !currentUsername().equals(hold.getCashierUsername()))) {
      model.addAttribute("cartError", msg("pos.error.holdNotFound"));
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : redirectWithCartError(msg("pos.error.holdNotFound"));
    }
    cart.clear();
    for (HeldSaleItem item : hold.getItems()) {
      Long productId = item.getProductId();
      if (productId == null && item.getProduct() != null) {
        productId = item.getProduct().getId();
      }
      if (productId == null) continue;
      cart.addItem(productId,
              item.getName(),
              item.getUnitPrice(),
              item.getQty(),
              item.getNote(),
              item.getPriceTier(),
              item.getUnitType(),
              item.getUnitSize() == null ? 1 : item.getUnitSize());
    }
    if (hold.getDiscountType() != null) {
      cart.setDiscountType(hold.getDiscountType());
      cart.setDiscountValue(hold.getDiscountValue() == null ? BigDecimal.ZERO : hold.getDiscountValue());
    } else {
      cart.setDiscount(hold.getDiscount() == null ? BigDecimal.ZERO : hold.getDiscount());
    }
    cart.setDiscountReason(hold.getDiscountReason());
    cart.setTaxRate(hold.getTaxRate() == null ? new BigDecimal("0.00") : hold.getTaxRate());
    if (hold.getCustomer() != null) {
      cart.setCustomerId(hold.getCustomer().getId());
    }
    posCartService.recordResumeHold(hold, cart);
    heldSaleRepo.delete(hold);
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/holds/{id}/delete")
  public String deleteHold(@PathVariable Long id,
                           @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                           @ModelAttribute("cart") Cart cart,
                           Model model) {
    HeldSale hold = heldSaleRepo.findById(id).orElse(null);
    if (hold != null && (currentUsername() == null || currentUsername().equals(hold.getCashierUsername()))) {
      heldSaleRepo.delete(hold);
    }
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/shift/open")
  public String openShift(@RequestParam Map<String, String> params,
                          @RequestParam(required = false) String terminalId,
                          @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                          @ModelAttribute("cart") Cart cart,
                          HttpServletRequest request,
                          Model model) {
    String username = currentUsername();
    if (username == null) {
      model.addAttribute("cartError", msg("pos.error.signInOpenShift"));
      enrichCartModel(model, cart, resolveTerminalId(null, request));
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/login";
    }
    try {
      Map<String, BigDecimal> openingByCurrency = parseCurrencyAmounts(params, "opening_");
      String resolvedTerminalId = resolveTerminalId(terminalId, request);
      shiftService.openShift(username, resolvedTerminalId, openingByCurrency);
      enrichCartModel(model, cart, resolvedTerminalId);
      model.addAttribute("shiftMessage", msg("shift.message.opened"));
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
    } catch (IllegalStateException ex) {
      model.addAttribute("cartError", ex.getMessage());
      enrichCartModel(model, cart, resolveTerminalId(terminalId, request));
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=" + encode(ex.getMessage());
    }
  }

  @PostMapping("/shift/cash-event")
  public String addShiftCashEvent(@RequestParam ShiftCashEventType eventType,
                                  @RequestParam(required = false) String currencyCode,
                                  @RequestParam(required = false) BigDecimal amount,
                                  @RequestParam(required = false) String reason,
                                  @RequestParam(required = false) String terminalId,
                                  @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                                  @ModelAttribute("cart") Cart cart,
                                  HttpServletRequest request,
                                  Model model) {
    String username = currentUsername();
    String resolvedTerminalId = resolveTerminalId(terminalId, request);
    try {
      shiftService.addCashEvent(username, resolvedTerminalId, eventType, currencyCode, amount, reason);
      enrichCartModel(model, cart, resolvedTerminalId);
      model.addAttribute("shiftMessage", msg("shift.message.cashMovementRecorded"));
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
    } catch (IllegalStateException ex) {
      model.addAttribute("cartError", ex.getMessage());
      enrichCartModel(model, cart, resolvedTerminalId);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=" + encode(ex.getMessage());
    }
  }

  @PostMapping("/shift/close")
  public String closeShift(@RequestParam Map<String, String> params,
                           @RequestParam(required = false) String notes,
                           @RequestParam(required = false) String terminalId,
                           @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                           @ModelAttribute("cart") Cart cart,
                           HttpServletRequest request,
                           Model model) {
    String username = currentUsername();
    String resolvedTerminalId = resolveTerminalId(terminalId, request);
    Map<String, BigDecimal> countedByCurrency = parseCurrencyAmounts(params, "counted_");
    try {
      ShiftService.ShiftCloseResult result = shiftService.closeShift(
              username,
              resolvedTerminalId,
              countedByCurrency,
              notes,
              hasManagerPrivileges()
      );
      BigDecimal totalSales = result.shift().getTotalSales() == null ? BigDecimal.ZERO : result.shift().getTotalSales();
      BigDecimal variance = result.shift().getVarianceCash() == null ? BigDecimal.ZERO : result.shift().getVarianceCash();
      String totalText = msg("shift.message.closedSummary",
              currencyService.getBaseCurrency().getSymbol() + totalSales.setScale(2, RoundingMode.HALF_UP),
              currencyService.getBaseCurrency().getSymbol() + variance.setScale(2, RoundingMode.HALF_UP));
      model.addAttribute("shiftMessage", totalText);
      enrichCartModel(model, cart, resolvedTerminalId);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
    } catch (IllegalStateException ex) {
      model.addAttribute("cartError", ex.getMessage());
      enrichCartModel(model, cart, resolvedTerminalId);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=" + encode(ex.getMessage());
    }
  }

  @GetMapping("/cart/update")
  public String updateQtyGet(@RequestParam(required = false) Long productId,
                             @RequestParam(required = false) Integer qty,
                             @ModelAttribute("cart") Cart cart,
                             Model model) {
    log.info("POS updateQty GET productId={} qty={}", productId, qty);
    if (productId == null || qty == null) {
      return "redirect:/pos?scanError=" + encode(msg("pos.error.invalidCartUpdate"));
    }
    return updateQty(productId, qty, null, cart, model);
  }

  @PostMapping("/cart/discount")
  public String updateDiscount(@RequestParam(required = false) DiscountType discountType,
                               @RequestParam(required = false) BigDecimal discountValue,
                               @RequestParam(required = false) String discountReason,
                               @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                               @ModelAttribute("cart") Cart cart, Model model) {
    posCartService.applyDiscount(cart, discountType, discountValue, discountReason);
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/cart/tax")
  public String updateTax(@RequestParam(required = false) BigDecimal taxRate,
                          @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                          @ModelAttribute("cart") Cart cart, Model model) {
    posCartService.applyTax(cart, taxRate);
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/cart/remove/{productId}")
  public String remove(@PathVariable Long productId,
                       @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                       @ModelAttribute("cart") Cart cart, Model model) {
    cart.remove(productId);
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  // Barcode scan endpoint: scans then adds to cart
  @PostMapping("/scan")
  public String scan(@RequestParam String barcode,
                     @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                     @ModelAttribute("cart") Cart cart,
                     Model model) {
    log.info("POS scan POST barcode='{}' hxRequest={}", barcode, hxRequest);
    if (barcode == null || barcode.isBlank()) {
      model.addAttribute("scanError", msg("pos.error.pleaseEnterBarcode"));
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartContainer" : "redirect:/pos?scanError=" + encode(msg("pos.error.pleaseEnterBarcode"));
    }
    String value = barcode.trim();
    Product p = productRepo.findByBarcode(value).orElse(null);
    if (p == null) {
      p = productRepo.findBySkuIgnoreCase(value).orElse(null);
    }
    if (p == null) {
      model.addAttribute("scanError", msg("pos.error.barcodeNotFound"));
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartContainer" : "redirect:/pos?scanError=" + encode(msg("pos.error.barcodeNotFound"));
    }
    String error = addProductToCart(p, cart);
    if (error != null) {
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", error);
        enrichCartModel(model, cart);
        return "pos/fragments :: cartPanel";
      }
      return redirectWithCartError(error);
    }
    model.addAttribute("scanError", null);
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartContainer" : "redirect:/pos";
  }

  @GetMapping("/scan")
  public String scanGet(@RequestParam(required = false) String barcode,
                        @ModelAttribute("cart") Cart cart,
                        Model model) {
    log.info("POS scan GET barcode='{}'", barcode);
    return scan(barcode, null, cart, model);
  }

  private boolean isHtmx(String hxRequest) {
    return hxRequest != null && !hxRequest.isBlank();
  }

  @ExceptionHandler(ResponseStatusException.class)
  public String handleResponseStatusException(ResponseStatusException ex,
                                              @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                                              @RequestHeader(value = "HX-Target", required = false) String hxTarget,
                                              @ModelAttribute("cart") Cart cart,
                                              Model model,
                                              HttpServletResponse response) {
    if (!isHtmx(hxRequest)) {
      throw ex;
    }
    String message = ex.getReason();
    if (message == null || message.isBlank()) {
      message = msg("pos.error.requestFailed");
    }
    model.addAttribute("cart", cart);
    model.addAttribute("cartError", message);
    response.setStatus(HttpStatus.OK.value());

    String target = normalizeHxTarget(hxTarget);
    if ("cartContainer".equals(target)) {
      return "pos/fragments :: cartContainer";
    }
    return "pos/fragments :: cartPanel";
  }

  private String normalizeHxTarget(String hxTarget) {
    if (hxTarget == null || hxTarget.isBlank()) return null;
    return hxTarget.startsWith("#") ? hxTarget.substring(1) : hxTarget;
  }

  private String addProductToCart(Product p, Cart cart) {
    if (p == null) {
      log.warn("POS product not found");
      return msg("pos.error.productNotFound");
    }
    CartItem existing = cart.getItem(p.getId());
    UnitType unitType = existing == null ? UnitType.PIECE : existing.getUnitType();
    int unitSize = existing == null ? 1 : existing.getUnitSize();
    int nextQty = existing == null ? 1 : existing.getQty() + 1;
    Customer customer = loadCustomer(cart.getCustomerId());
    PriceTier priceTier = autoPriceTier(p, customer, nextQty, unitSize);
    String error = validateSaleable(p, priceTier, unitType);
    if (error != null) return error;
    BigDecimal unitPrice = resolveUnitPrice(p, priceTier, unitSize);
    cart.add(p, priceTier, unitType, unitSize, unitPrice);
    applyAutoPricing(cart, p, customer);
    return null;
  }

  private void applyAutoPricing(Cart cart, Product p, Customer customer) {
    if (p == null) return;
    CartItem item = cart.getItem(p.getId());
    if (item == null) return;
    int unitSize = item.getUnitSize();
    PriceTier priceTier = autoPriceTier(p, customer, item.getQty(), unitSize);
    BigDecimal unitPrice = resolveUnitPrice(p, priceTier, unitSize);
    if (unitPrice != null) {
      cart.setPriceTier(p.getId(), priceTier, unitPrice);
    }
  }

  private void applyAutoPricing(Cart cart, Customer customer) {
    for (CartItem item : cart.getItems()) {
      Product p = productRepo.findById(item.getProductId()).orElse(null);
      if (p == null) continue;
      int unitSize = item.getUnitSize();
      PriceTier priceTier = autoPriceTier(p, customer, item.getQty(), unitSize);
      BigDecimal unitPrice = resolveUnitPrice(p, priceTier, unitSize);
      if (unitPrice != null) {
        cart.setPriceTier(item.getProductId(), priceTier, unitPrice);
      }
    }
  }

  private PriceTier autoPriceTier(Product p, Customer customer, int qty, int unitSize) {
    if (p == null) return PriceTier.RETAIL;
    boolean wholesaleCustomer = customer != null && Boolean.TRUE.equals(customer.getWholesale());
    int pieces = Math.max(0, qty) * safeUnitSize(unitSize);
    Integer minQty = p.getWholesaleMinQty();
    boolean meetsThreshold = minQty != null && minQty > 0 && pieces >= minQty;
    if ((wholesaleCustomer || meetsThreshold) && p.getWholesalePrice() != null) {
      return PriceTier.WHOLESALE;
    }
    return PriceTier.RETAIL;
  }

  private Integer resolveUnitSizeForSelection(Product p, UnitType unitType) {
    if (unitType == null || unitType == UnitType.PIECE) return 1;
    if (p == null) return null;
    if (unitType == UnitType.BOX) {
      Integer size = p.getUnitsPerBox();
      return size != null && size > 0 ? size : null;
    }
    Integer size = p.getUnitsPerCase();
    return size != null && size > 0 ? size : null;
  }

  private int safeUnitSize(Integer unitSize) {
    return unitSize == null || unitSize <= 0 ? 1 : unitSize;
  }

  private String validateSaleable(Product p, PriceTier priceTier, UnitType unitType) {
    if (p == null) {
      log.warn("POS product not found");
      return msg("pos.error.productNotFound");
    }
    if (Boolean.FALSE.equals(p.getActive())) {
      log.warn("POS product {} is inactive", p.getId());
      return msg("pos.error.productInactive");
    }
    if (unitType == UnitType.BOX && (p.getUnitsPerBox() == null || p.getUnitsPerBox() <= 0)) {
      return msg("pos.error.boxSizeNotSet");
    }
    if (unitType == UnitType.CASE && (p.getUnitsPerCase() == null || p.getUnitsPerCase() <= 0)) {
      return msg("pos.error.caseSizeNotSet");
    }
    int unitSize = unitType == UnitType.BOX
            ? safeUnitSize(p.getUnitsPerBox())
            : unitType == UnitType.CASE
            ? safeUnitSize(p.getUnitsPerCase())
            : 1;
    BigDecimal price = resolveUnitPrice(p, priceTier, unitSize);
    if (price == null) {
      log.warn("POS product {} has no {} price set", p.getId(), priceTier);
      return priceTier == PriceTier.WHOLESALE ? msg("pos.error.wholesalePriceNotSet") : msg("pos.error.noPrice");
    }
    return null;
  }

  private BigDecimal resolveUnitPrice(Product p, PriceTier priceTier, int unitSize) {
    if (p == null) return null;
    BigDecimal base = priceTier == PriceTier.WHOLESALE ? p.getWholesalePrice() : p.getPrice();
    if (base == null) return null;
    return base.multiply(BigDecimal.valueOf(safeUnitSize(unitSize)));
  }

  private String redirectWithCartError(String message) {
    return "redirect:/pos?cartError=" + encode(message);
  }

  private String msg(String key, Object... args) {
    return i18nService.msg(key, args);
  }

  private String encode(String message) {
    return org.springframework.web.util.UriUtils.encode(
            message == null ? "" : message,
            java.nio.charset.StandardCharsets.UTF_8
    );
  }

  private void enrichCartModel(Model model, Cart cart) {
    enrichCartModel(model, cart, null);
  }

  private void enrichCartModel(Model model, Cart cart, String terminalId) {
    String preferredTerminalId = sanitizeTerminalId(terminalId);
    if (preferredTerminalId == null) {
      preferredTerminalId = terminalSettingsService.preferredTerminalId();
    }
    TerminalSettings terminalSettings = terminalSettingsService.resolveForTerminal(preferredTerminalId);
    model.addAttribute("terminalSettings", terminalSettings);
    model.addAttribute("preferredTerminalId", terminalSettings.getTerminalId());
    model.addAttribute("cart", cart);
    model.addAttribute("baseCurrency", currencyService.getBaseCurrency());
    model.addAttribute("currencies", currencyService.getActiveCurrencies());
    model.addAttribute("shiftVarianceThreshold", shiftService.varianceThreshold());
    String username = currentUsername();
    if (username != null) {
      model.addAttribute("holds", heldSaleRepo.findByCashierUsernameOrderByCreatedAtDesc(username));
      Shift openShift = findOpenShift(username, terminalId);
      model.addAttribute("openShift", openShift);
      if (openShift != null) {
        model.addAttribute("shiftCashEvents", shiftService.listCashEvents(openShift.getId()));
        Map<String, BigDecimal> openingAmounts = shiftService.parseAmounts(openShift.getOpeningFloatJson());
        ShiftService.ShiftReconciliationData preview = shiftService.previewReconciliation(openShift, Map.of());
        Map<String, BigDecimal> expectedAmounts = shiftService.parseAmounts(openShift.getExpectedAmountsJson());
        if (expectedAmounts.isEmpty() && openShift.getStatus() == ShiftStatus.OPEN) {
          expectedAmounts = preview.expectedByCurrency();
        }
        model.addAttribute("shiftPreview", preview);
        model.addAttribute("shiftOpeningAmounts", openingAmounts);
        model.addAttribute("shiftExpectedAmounts", expectedAmounts);
        model.addAttribute("shiftCountedAmounts", shiftService.parseAmounts(openShift.getCountedAmountsJson()));
        model.addAttribute("shiftVarianceAmounts", shiftService.parseAmounts(openShift.getVarianceAmountsJson()));
        model.addAttribute("terminalId", sanitizeTerminalId(terminalId) != null ? sanitizeTerminalId(terminalId) : openShift.getTerminalId());
      } else {
        model.addAttribute("shiftCashEvents", List.of());
        model.addAttribute("shiftPreview", ShiftService.ShiftReconciliationData.empty());
        model.addAttribute("shiftOpeningAmounts", Map.of());
        model.addAttribute("shiftExpectedAmounts", Map.of());
        model.addAttribute("shiftCountedAmounts", Map.of());
        model.addAttribute("shiftVarianceAmounts", Map.of());
        model.addAttribute("terminalId", preferredTerminalId);
      }
    } else {
      model.addAttribute("holds", List.of());
      model.addAttribute("openShift", null);
      model.addAttribute("shiftCashEvents", List.of());
      model.addAttribute("shiftPreview", ShiftService.ShiftReconciliationData.empty());
      model.addAttribute("shiftOpeningAmounts", Map.of());
      model.addAttribute("shiftExpectedAmounts", Map.of());
      model.addAttribute("shiftCountedAmounts", Map.of());
      model.addAttribute("shiftVarianceAmounts", Map.of());
      model.addAttribute("terminalId", preferredTerminalId);
    }
    if (cart.getCustomerId() != null) {
      customerRepo.findById(cart.getCustomerId()).ifPresent(c -> model.addAttribute("currentCustomer", c));
    } else {
      model.addAttribute("currentCustomer", null);
    }
  }

  private Shift findOpenShift(String username) {
    return findOpenShift(username, null);
  }

  private Shift findOpenShift(String username, String terminalId) {
    if (username == null || username.isBlank()) return null;
    return shiftService.findOpenShift(username, sanitizeTerminalId(terminalId)).orElse(null);
  }

  private Map<String, BigDecimal> parseCurrencyAmounts(Map<String, String> params, String prefix) {
    Map<String, BigDecimal> values = new LinkedHashMap<>();
    if (params == null || prefix == null || prefix.isBlank()) return values;
    for (Map.Entry<String, String> entry : params.entrySet()) {
      String key = entry.getKey();
      if (key == null || !key.startsWith(prefix)) continue;
      String currency = key.substring(prefix.length()).trim().toUpperCase();
      if (currency.isBlank()) continue;
      BigDecimal parsed = parseAmount(entry.getValue());
      if (parsed == null) continue;
      values.put(currency, parsed);
    }
    return values;
  }

  private BigDecimal parseAmount(String raw) {
    if (raw == null || raw.isBlank()) return null;
    try {
      BigDecimal value = new BigDecimal(raw.trim());
      if (value.compareTo(BigDecimal.ZERO) < 0) return BigDecimal.ZERO;
      return value;
    } catch (NumberFormatException ex) {
      return null;
    }
  }

  private String resolveTerminalId(String terminalId, HttpServletRequest request) {
    String explicit = sanitizeTerminalId(terminalId);
    if (explicit != null) return explicit;
    if (request == null) return null;
    String fromHeader = sanitizeTerminalId(request.getHeader("X-Terminal-Id"));
    if (fromHeader != null) return fromHeader;
    String fallbackHeader = sanitizeTerminalId(request.getHeader("X-POS-Terminal"));
    if (fallbackHeader != null) return fallbackHeader;
    return terminalSettingsService.preferredTerminalId();
  }

  private String sanitizeTerminalId(String terminalId) {
    if (terminalId == null) return null;
    String trimmed = terminalId.trim();
    if (trimmed.isEmpty()) return null;
    return trimmed.length() <= 128 ? trimmed : trimmed.substring(0, 128);
  }

  private boolean hasManagerPrivileges() {
    Authentication auth = SecurityContextHolder.getContext().getAuthentication();
    if (auth == null || !auth.isAuthenticated()) return false;
    return auth.getAuthorities().stream().anyMatch(a -> {
      String authority = a.getAuthority();
      if (authority == null) return false;
      return "ROLE_ADMIN".equals(authority) || "ROLE_MANAGER".equals(authority);
    });
  }

  private Customer loadCustomer(Long customerId) {
    if (customerId == null) return null;
    return customerRepo.findById(customerId).orElse(null);
  }

  private void addSplitPayment(List<SalePayment> payments, PaymentMethod method, BigDecimal foreignAmount,
                               String currencyCode, Currency baseCurrency) {
    if (method == null || foreignAmount == null) return;
    if (foreignAmount.compareTo(BigDecimal.ZERO) <= 0) return;
    Currency currency = null;
    if (currencyCode != null && !currencyCode.isBlank()) {
      Currency found = currencyService.findByCode(currencyCode);
      if (found != null && Boolean.TRUE.equals(found.getActive())) {
        currency = found;
      }
    }
    Currency effectiveCurrency = currency != null ? currency : baseCurrency;
    BigDecimal rate = effectiveCurrency != null && effectiveCurrency.getRateToBase() != null
            ? effectiveCurrency.getRateToBase()
            : BigDecimal.ONE;
    if (rate.compareTo(BigDecimal.ZERO) <= 0) rate = BigDecimal.ONE;
    BigDecimal baseAmount = foreignAmount.multiply(rate).setScale(2, RoundingMode.HALF_UP);

    SalePayment payment = new SalePayment();
    payment.setMethod(method);
    payment.setAmount(baseAmount);
    if (effectiveCurrency != null) {
      payment.setCurrencyCode(effectiveCurrency.getCode());
      payment.setCurrencyRate(rate);
      payment.setForeignAmount(foreignAmount.setScale(2, RoundingMode.HALF_UP));
    }
    payments.add(payment);
  }

  private SalePayment buildPayment(PaymentMethod method, BigDecimal baseTotal,
                                   String currencyCode, BigDecimal currencyRate, BigDecimal foreignAmount) {
    SalePayment payment = new SalePayment();
    payment.setMethod(method);
    Currency baseCurrency = currencyService.getBaseCurrency();

    BigDecimal baseAmount = baseTotal == null ? BigDecimal.ZERO : baseTotal.setScale(2, RoundingMode.HALF_UP);
    if (method == PaymentMethod.CASH) {
      CashTenderData cashTender = resolveCashTender(currencyCode, currencyRate, foreignAmount);
      payment.setCurrencyCode(cashTender.currencyCode());
      payment.setCurrencyRate(cashTender.rateToBase());
      payment.setForeignAmount(cashTender.receivedForeignAmount());
    } else if (baseCurrency != null) {
      payment.setCurrencyCode(baseCurrency.getCode());
      payment.setCurrencyRate(BigDecimal.ONE);
      payment.setForeignAmount(baseAmount.setScale(2, RoundingMode.HALF_UP));
    }
    payment.setAmount(baseAmount.setScale(2, RoundingMode.HALF_UP));
    return payment;
  }

  @PostMapping("/checkout")
  public String checkout(@RequestParam PaymentMethod method,
                         @RequestParam(required = false) String currencyCode,
                         @RequestParam(required = false) BigDecimal currencyRate,
                         @RequestParam(required = false) BigDecimal foreignAmount,
                         @RequestParam(required = false) String terminalId,
                         @RequestParam(required = false) String clientCheckoutId,
                         @ModelAttribute("cart") Cart cart,
                         SessionStatus sessionStatus,
                         RedirectAttributes redirectAttributes,
                          @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                         HttpServletRequest request,
                         Model model) {
    String resolvedTerminalId = resolveTerminalId(terminalId, request);
    if (cart.getItems().isEmpty()) {
      if (isHtmx(hxRequest)) {
        model.addAttribute("scanError", msg("pos.error.cartEmpty"));
        enrichCartModel(model, cart, resolvedTerminalId);
        return "pos/fragments :: cartContainer";
      }
      return "redirect:/pos?scanError=" + encode(msg("pos.error.cartEmpty"));
    }
    try {
      BigDecimal expectedTotal = cart.getTotal() == null ? BigDecimal.ZERO : cart.getTotal().setScale(2, RoundingMode.HALF_UP);
      if (method == PaymentMethod.CASH && foreignAmount == null) {
        throw new IllegalStateException(msg("pos.error.enterCashReceived"));
      }
      if (method == PaymentMethod.CASH) {
        CashTenderData cashTender = resolveCashTender(currencyCode, currencyRate, foreignAmount);
        if (cashTender.receivedBaseAmount().compareTo(expectedTotal) < 0) {
          throw new IllegalStateException(msg("pos.error.cashLessThanTotal"));
        }
      }
      SalePayment payment = buildPayment(method, cart.getTotal(), currencyCode, currencyRate, foreignAmount);
      String username = currentUsername();
      Shift openShift = findOpenShift(username, resolvedTerminalId);
      if (openShift == null) {
        if (isHtmx(hxRequest)) {
          model.addAttribute("cartError", msg("pos.error.openShiftBeforeCheckout"));
          enrichCartModel(model, cart, resolvedTerminalId);
          return "pos/fragments :: cartContainer";
        }
        return "redirect:/pos?cartError=" + encode(msg("pos.error.openShiftBeforeCheckout"));
      }
      if (openShift.getTerminalId() != null && !openShift.getTerminalId().isBlank()) {
        resolvedTerminalId = openShift.getTerminalId();
      }
      Customer customer = loadCustomer(cart.getCustomerId());
      String checkoutTerminalId = resolvedTerminalId;
      CheckoutAttemptService.CheckoutResult result = checkoutAttemptService.process(
              clientCheckoutId,
              checkoutTerminalId,
              () -> posService.checkout(cart, payment, username, customer, openShift, checkoutTerminalId)
      );
      Sale sale = result.sale();
      TerminalSettings terminalSettings = terminalSettingsService.resolveForTerminal(resolvedTerminalId);
      sessionStatus.setComplete(); // clears session cart
      if (isHtmx(hxRequest)) {
        Cart fresh = new Cart();
        enrichCartModel(model, fresh, resolvedTerminalId);
        model.addAttribute("checkoutSuccess",
                result.replayed() ? msg("pos.checkoutReplayed") : msg("pos.receiptReady"));
        model.addAttribute("receiptUrl", "/sales/" + sale.getId() + "/receipt");
        model.addAttribute("checkoutSaleId", sale.getId());
        model.addAttribute("checkoutAutoPrint", terminalSettings.getAutoPrintEnabled() != null && terminalSettings.getAutoPrintEnabled());
        return "pos/fragments :: cartContainer";
      }
      redirectAttributes.addFlashAttribute("successMessage",
              result.replayed() ? msg("pos.checkoutReplayed") : msg("pos.receiptReady"));
      return "redirect:/sales/" + sale.getId() + "/receipt";
    } catch (IllegalStateException ex) {
      String message = ex.getMessage() == null ? msg("pos.checkoutFailed") : ex.getMessage();
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", message);
        enrichCartModel(model, cart, resolvedTerminalId);
        return "pos/fragments :: cartContainer";
      }
      return "redirect:/pos?cartError=" + encode(message);
    }
  }

  private CashTenderData resolveCashTender(String currencyCode, BigDecimal currencyRate, BigDecimal foreignAmount) {
    Currency baseCurrency = currencyService.getBaseCurrency();
    BigDecimal effectiveRate = null;
    String effectiveCode = null;

    if (currencyCode != null && !currencyCode.isBlank()) {
      Currency currency = currencyService.findByCode(currencyCode);
      if (currency != null && Boolean.TRUE.equals(currency.getActive())) {
        BigDecimal rateToBase = currency.getRateToBase();
        if (rateToBase != null && rateToBase.compareTo(BigDecimal.ZERO) > 0) {
          effectiveRate = rateToBase;
          effectiveCode = currency.getCode();
        }
      }
    }

    if (effectiveRate == null && currencyRate != null && currencyRate.compareTo(BigDecimal.ZERO) > 0) {
      effectiveRate = currencyRate;
      if (currencyCode != null && !currencyCode.isBlank()) {
        effectiveCode = currencyCode.trim().toUpperCase();
      }
    }

    if (effectiveRate == null) {
      effectiveRate = BigDecimal.ONE;
      if (baseCurrency != null) {
        effectiveCode = baseCurrency.getCode();
      }
    } else if (effectiveCode == null && baseCurrency != null) {
      effectiveCode = baseCurrency.getCode();
    }

    BigDecimal safeForeign = foreignAmount == null ? BigDecimal.ZERO : foreignAmount.max(BigDecimal.ZERO);
    BigDecimal normalizedForeign = safeForeign.setScale(2, RoundingMode.HALF_UP);
    BigDecimal receivedBase = normalizedForeign.multiply(effectiveRate).setScale(2, RoundingMode.HALF_UP);
    return new CashTenderData(effectiveCode, effectiveRate, normalizedForeign, receivedBase);
  }

  private record CashTenderData(String currencyCode,
                                BigDecimal rateToBase,
                                BigDecimal receivedForeignAmount,
                                BigDecimal receivedBaseAmount) {
  }

  @PostMapping("/checkout/split")
  public String checkoutSplit(@RequestParam(required = false) PaymentMethod method1,
                              @RequestParam(required = false) BigDecimal amount1,
                              @RequestParam(required = false) String currencyCode1,
                              @RequestParam(required = false) PaymentMethod method2,
                              @RequestParam(required = false) BigDecimal amount2,
                              @RequestParam(required = false) String currencyCode2,
                              @RequestParam(required = false) PaymentMethod method3,
                              @RequestParam(required = false) BigDecimal amount3,
                              @RequestParam(required = false) String currencyCode3,
                              @RequestParam(required = false) String terminalId,
                              @RequestParam(required = false) String clientCheckoutId,
                              @ModelAttribute("cart") Cart cart,
                              SessionStatus sessionStatus,
                              RedirectAttributes redirectAttributes,
                              @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                              HttpServletRequest request,
                              Model model) {
    String resolvedTerminalId = resolveTerminalId(terminalId, request);
    if (cart.getItems().isEmpty()) {
      if (isHtmx(hxRequest)) {
        model.addAttribute("scanError", msg("pos.error.cartEmpty"));
        enrichCartModel(model, cart, resolvedTerminalId);
        return "pos/fragments :: cartContainer";
      }
      return "redirect:/pos?scanError=" + encode(msg("pos.error.cartEmpty"));
    }
    List<SalePayment> payments = new ArrayList<>();
    Currency baseCurrency = currencyService.getBaseCurrency();
    addSplitPayment(payments, method1, amount1, currencyCode1, baseCurrency);
    addSplitPayment(payments, method2, amount2, currencyCode2, baseCurrency);
    addSplitPayment(payments, method3, amount3, currencyCode3, baseCurrency);
    if (payments.isEmpty()) {
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", msg("pos.error.noPayments"));
        enrichCartModel(model, cart, resolvedTerminalId);
        return "pos/fragments :: cartContainer";
      }
      return "redirect:/pos?cartError=" + encode(msg("pos.error.noPayments"));
    }
    BigDecimal total = payments.stream()
            .map(SalePayment::getAmount)
            .reduce(BigDecimal.ZERO, BigDecimal::add);
    BigDecimal expectedTotal = cart.getTotal() == null ? BigDecimal.ZERO : cart.getTotal();
    if (total.subtract(expectedTotal).abs().compareTo(new BigDecimal("0.01")) > 0) {
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", msg("pos.error.splitTotalMismatch"));
        enrichCartModel(model, cart, resolvedTerminalId);
        return "pos/fragments :: cartContainer";
      }
      return "redirect:/pos?cartError=" + encode(msg("pos.error.splitTotalMismatch"));
    }
    try {
      String username = currentUsername();
      Shift openShift = findOpenShift(username, resolvedTerminalId);
      if (openShift == null) {
        if (isHtmx(hxRequest)) {
          model.addAttribute("cartError", msg("pos.error.openShiftBeforeCheckout"));
          enrichCartModel(model, cart, resolvedTerminalId);
          return "pos/fragments :: cartContainer";
        }
        return "redirect:/pos?cartError=" + encode(msg("pos.error.openShiftBeforeCheckout"));
      }
      if (openShift.getTerminalId() != null && !openShift.getTerminalId().isBlank()) {
        resolvedTerminalId = openShift.getTerminalId();
      }
      Customer customer = loadCustomer(cart.getCustomerId());
      String checkoutTerminalId = resolvedTerminalId;
      CheckoutAttemptService.CheckoutResult result = checkoutAttemptService.process(
              clientCheckoutId,
              checkoutTerminalId,
              () -> posService.checkoutSplit(cart, payments, username, customer, openShift, checkoutTerminalId)
      );
      Sale sale = result.sale();
      TerminalSettings terminalSettings = terminalSettingsService.resolveForTerminal(resolvedTerminalId);
      sessionStatus.setComplete();
      if (isHtmx(hxRequest)) {
        Cart fresh = new Cart();
        enrichCartModel(model, fresh, resolvedTerminalId);
        model.addAttribute("checkoutSuccess",
                result.replayed() ? msg("pos.checkoutReplayed") : msg("pos.receiptReady"));
        model.addAttribute("receiptUrl", "/sales/" + sale.getId() + "/receipt");
        model.addAttribute("checkoutSaleId", sale.getId());
        model.addAttribute("checkoutAutoPrint", terminalSettings.getAutoPrintEnabled() != null && terminalSettings.getAutoPrintEnabled());
        return "pos/fragments :: cartContainer";
      }
      redirectAttributes.addFlashAttribute("successMessage",
              result.replayed() ? msg("pos.checkoutReplayed") : msg("pos.receiptReady"));
      return "redirect:/sales/" + sale.getId() + "/receipt";
    } catch (IllegalStateException ex) {
      String message = ex.getMessage() == null ? msg("pos.checkoutFailed") : ex.getMessage();
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", message);
        enrichCartModel(model, cart, resolvedTerminalId);
        return "pos/fragments :: cartContainer";
      }
      return "redirect:/pos?cartError=" + encode(message);
    }
  }

  @PostMapping(value = "/checkout/{saleId}/print", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public PosHardwareService.PrintResponse printReceipt(@PathVariable Long saleId,
                                                       @RequestParam(required = false) String terminalId,
                                                       @RequestParam(required = false, defaultValue = "false") boolean reprint,
                                                       HttpServletRequest request) {
    String resolvedTerminalId = resolveTerminalId(terminalId, request);
    return posHardwareService.buildReceiptPrintResponse(saleId, resolvedTerminalId, reprint);
  }

  @PostMapping(value = "/drawer/open", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public PosHardwareService.DrawerResponse openDrawer(@RequestParam(required = false) String terminalId,
                                                      @RequestParam(required = false) Long saleId,
                                                      HttpServletRequest request) {
    String actor = currentUsername();
    if (actor == null) {
      throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, msg("pos.error.signInOpenDrawer"));
    }
    String resolvedTerminalId = resolveTerminalId(terminalId, request);
    return posHardwareService.openDrawer(actor, resolvedTerminalId, saleId);
  }

  private long nanosToMillis(long nanos) {
    return Math.max(0L, nanos / 1_000_000L);
  }

  private String currentUsername() {
    Authentication auth = SecurityContextHolder.getContext().getAuthentication();
    if (auth == null || !auth.isAuthenticated()) return null;
    String name = auth.getName();
    if (name == null || "anonymousUser".equalsIgnoreCase(name)) return null;
    return name;
  }

  private CartItem copyCartItem(CartItem item) {
    if (item == null) return null;
    CartItem copy = new CartItem(item.getProductId(), item.getName(), item.getUnitPrice(), item.getQty(),
            item.getPriceTier(), item.getUnitType(), item.getUnitSize());
    copy.setNote(item.getNote());
    return copy;
  }

  public record ProductFeedResponse(
          List<ProductFeedService.ProductFeedItem> items,
          String nextCursor,
          boolean hasMore,
          Long total
  ) {
  }
}
