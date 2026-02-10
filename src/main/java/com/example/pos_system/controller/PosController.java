package com.example.pos_system.controller;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.dto.CartItem;
import com.example.pos_system.entity.*;
import com.example.pos_system.repository.*;
import com.example.pos_system.service.PosService;

import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.*;
import org.springframework.http.HttpStatus;
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
import jakarta.servlet.http.HttpServletResponse;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

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
  private final ShiftRepo shiftRepo;
  private final SaleRepo saleRepo;

  public PosController(ProductRepo productRepo,
                       CategoryRepo categoryRepo,
                       PosService posService,
                       HeldSaleRepo heldSaleRepo,
                       CustomerRepo customerRepo,
                       ShiftRepo shiftRepo,
                       SaleRepo saleRepo) {
    this.productRepo = productRepo;
    this.categoryRepo = categoryRepo;
    this.posService = posService;
    this.heldSaleRepo = heldSaleRepo;
    this.customerRepo = customerRepo;
    this.shiftRepo = shiftRepo;
    this.saleRepo = saleRepo;
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
    int pageNum = page == null ? 0 : Math.max(0, page);
    Page<Product> products = loadProductsPage(q, categoryId, pageNum);
    model.addAttribute("products", products.getContent());
    model.addAttribute("hasNext", products.hasNext());
    model.addAttribute("page", pageNum);
    model.addAttribute("nextPage", pageNum + 1);
    model.addAttribute("prevPage", Math.max(0, pageNum - 1));
    model.addAttribute("q", q);
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
    Page<Product> products = loadProductsPage(q, categoryId, page);
    model.addAttribute("products", products.getContent());
    model.addAttribute("hasNext", products.hasNext());
    model.addAttribute("nextPage", products.getNumber() + 1);
    if (Boolean.TRUE.equals(append)) {
      return "pos/fragments :: productGridItems";
    }
    return "pos/fragments :: productGrid";
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
      model.addAttribute("cartError", "Enter a SKU or barcode to add.");
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=Enter+a+SKU+or+barcode+to+add.";
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
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=" + org.springframework.web.util.UriUtils.encode(error, java.nio.charset.StandardCharsets.UTF_8);
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
      String error = "Product not found.";
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
      String error = unitType == UnitType.BOX ? "Box size not set." : "Case size not set.";
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", error);
        enrichCartModel(model, cart);
        return "pos/fragments :: cartPanel";
      }
      return redirectWithCartError(error);
    }
    Customer customer = loadCustomer(cart.getCustomerId());
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
      model.addAttribute("cartError", "Enter a phone number or email.");
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=Enter+a+phone+number+or+email.";
    }
    String value = query.trim();
    Customer customer = customerRepo.findByPhone(value)
            .orElseGet(() -> customerRepo.findByEmail(value).orElse(null));
    if (customer == null) {
      model.addAttribute("cartError", "Customer not found.");
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=Customer+not+found.";
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
      model.addAttribute("cartError", "Customer name is required.");
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=Customer+name+is+required.";
    }
    Customer customer = new Customer();
    customer.setName(name.trim());
    if (phone != null && !phone.isBlank()) customer.setPhone(phone.trim());
    if (email != null && !email.isBlank()) customer.setEmail(email.trim());
    customer.setWholesale(Boolean.TRUE.equals(wholesale));
    try {
      customer = customerRepo.save(customer);
    } catch (DataIntegrityViolationException ex) {
      model.addAttribute("cartError", "Phone or email already exists.");
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=Phone+or+email+already+exists.";
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
      model.addAttribute("cartError", "Cart is empty.");
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=Cart+is+empty.";
    }
    HeldSale hold = new HeldSale();
    hold.setCashierUsername(currentUsername());
    hold.setCreatedAt(LocalDateTime.now());
    hold.setLabel(label == null || label.isBlank() ? "Held sale" : label.trim());
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
    heldSaleRepo.save(hold);
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
      model.addAttribute("cartError", "Hold not found.");
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=Hold+not+found.";
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
  public String openShift(@RequestParam(required = false) BigDecimal openingCash,
                          @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                          @ModelAttribute("cart") Cart cart,
                          Model model) {
    String username = currentUsername();
    if (username == null) {
      model.addAttribute("cartError", "Sign in to open a shift.");
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/login";
    }
    if (findOpenShift(username) != null) {
      model.addAttribute("cartError", "Shift already open.");
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=Shift+already+open.";
    }
    Shift shift = new Shift();
    shift.setCashierUsername(username);
    shift.setOpenedAt(LocalDateTime.now());
    shift.setOpeningCash(openingCash == null ? BigDecimal.ZERO : openingCash.max(BigDecimal.ZERO));
    shift.setStatus(ShiftStatus.OPEN);
    shiftRepo.save(shift);
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/shift/close")
  public String closeShift(@RequestParam(required = false) BigDecimal closingCash,
                           @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                           @ModelAttribute("cart") Cart cart,
                           Model model) {
    String username = currentUsername();
    Shift shift = findOpenShift(username);
    if (shift == null) {
      model.addAttribute("cartError", "No open shift to close.");
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos?cartError=No+open+shift+to+close.";
    }
    shift.setClosedAt(LocalDateTime.now());
    shift.setClosingCash(closingCash == null ? BigDecimal.ZERO : closingCash.max(BigDecimal.ZERO));
    shift.setStatus(ShiftStatus.CLOSED);
    List<Sale> sales = saleRepo.findByShift_Id(shift.getId());
    BigDecimal totalSales = BigDecimal.ZERO;
    BigDecimal cashTotal = BigDecimal.ZERO;
    BigDecimal cardTotal = BigDecimal.ZERO;
    BigDecimal qrTotal = BigDecimal.ZERO;
    for (Sale sale : sales) {
      if (sale.getStatus() == SaleStatus.VOID) continue;
      BigDecimal net = safeNetTotal(sale);
      totalSales = totalSales.add(net);
      if (sale.getPayments() != null && !sale.getPayments().isEmpty()) {
        for (SalePayment payment : sale.getPayments()) {
          if (payment.getMethod() == PaymentMethod.CASH) cashTotal = cashTotal.add(payment.getAmount());
          if (payment.getMethod() == PaymentMethod.CARD) cardTotal = cardTotal.add(payment.getAmount());
          if (payment.getMethod() == PaymentMethod.QR) qrTotal = qrTotal.add(payment.getAmount());
        }
      } else if (sale.getPaymentMethod() != null) {
        if (sale.getPaymentMethod() == PaymentMethod.CASH) cashTotal = cashTotal.add(net);
        if (sale.getPaymentMethod() == PaymentMethod.CARD) cardTotal = cardTotal.add(net);
        if (sale.getPaymentMethod() == PaymentMethod.QR) qrTotal = qrTotal.add(net);
      }
    }
    shift.setTotalSales(totalSales);
    shift.setCashTotal(cashTotal);
    shift.setCardTotal(cardTotal);
    shift.setQrTotal(qrTotal);
    shiftRepo.save(shift);
    enrichCartModel(model, cart);
    model.addAttribute("shiftMessage", "Shift closed. Total sales: $" + totalSales.setScale(2, java.math.RoundingMode.HALF_UP));
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @GetMapping("/cart/update")
  public String updateQtyGet(@RequestParam(required = false) Long productId,
                             @RequestParam(required = false) Integer qty,
                             @ModelAttribute("cart") Cart cart,
                             Model model) {
    log.info("POS updateQty GET productId={} qty={}", productId, qty);
    if (productId == null || qty == null) {
      return "redirect:/pos?scanError=Invalid+cart+update";
    }
    return updateQty(productId, qty, null, cart, model);
  }

  @PostMapping("/cart/discount")
  public String updateDiscount(@RequestParam(required = false) DiscountType discountType,
                               @RequestParam(required = false) BigDecimal discountValue,
                               @RequestParam(required = false) String discountReason,
                               @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                               @ModelAttribute("cart") Cart cart, Model model) {
    DiscountType safeType = discountType == null ? DiscountType.AMOUNT : discountType;
    BigDecimal safeValue = discountValue == null ? BigDecimal.ZERO : discountValue.max(BigDecimal.ZERO);
    if (safeType == DiscountType.PERCENT && safeValue.compareTo(new BigDecimal("100")) > 0) {
      safeValue = new BigDecimal("100");
    }
    if (safeType == DiscountType.AMOUNT) {
      BigDecimal subtotal = cart.getSubtotal();
      if (subtotal != null && safeValue.compareTo(subtotal) > 0) {
        safeValue = subtotal;
      }
    }
    cart.setDiscountType(safeType);
    cart.setDiscountValue(safeValue);
    cart.setDiscountReason(discountReason);
    enrichCartModel(model, cart);
    return isHtmx(hxRequest) ? "pos/fragments :: cartPanel" : "redirect:/pos";
  }

  @PostMapping("/cart/tax")
  public String updateTax(@RequestParam(required = false) BigDecimal taxRate,
                          @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                          @ModelAttribute("cart") Cart cart, Model model) {
    BigDecimal safeRate = taxRate == null ? BigDecimal.ZERO : taxRate.max(BigDecimal.ZERO);
    if (safeRate.compareTo(new BigDecimal("100")) > 0) {
      safeRate = new BigDecimal("100");
    }
    BigDecimal rate = safeRate.divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP);
    cart.setTaxRate(rate);
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
      model.addAttribute("scanError", "Please enter a barcode.");
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartContainer" : "redirect:/pos?scanError=Please+enter+a+barcode";
    }
    String value = barcode.trim();
    Product p = productRepo.findByBarcode(value).orElse(null);
    if (p == null) {
      p = productRepo.findBySkuIgnoreCase(value).orElse(null);
    }
    if (p == null) {
      model.addAttribute("scanError", "Barcode not found.");
      enrichCartModel(model, cart);
      return isHtmx(hxRequest) ? "pos/fragments :: cartContainer" : "redirect:/pos?scanError=Barcode+not+found";
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
      message = "Request failed";
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
      return "Product not found.";
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
      return "Product not found.";
    }
    if (Boolean.FALSE.equals(p.getActive())) {
      log.warn("POS product {} is inactive", p.getId());
      return "Product is inactive.";
    }
    if (unitType == UnitType.BOX && (p.getUnitsPerBox() == null || p.getUnitsPerBox() <= 0)) {
      return "Box size not set.";
    }
    if (unitType == UnitType.CASE && (p.getUnitsPerCase() == null || p.getUnitsPerCase() <= 0)) {
      return "Case size not set.";
    }
    int unitSize = unitType == UnitType.BOX
            ? safeUnitSize(p.getUnitsPerBox())
            : unitType == UnitType.CASE
            ? safeUnitSize(p.getUnitsPerCase())
            : 1;
    BigDecimal price = resolveUnitPrice(p, priceTier, unitSize);
    if (price == null) {
      log.warn("POS product {} has no {} price set", p.getId(), priceTier);
      return priceTier == PriceTier.WHOLESALE ? "Wholesale price not set." : "Product has no price set.";
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
    String encoded = org.springframework.web.util.UriUtils.encode(message, java.nio.charset.StandardCharsets.UTF_8);
    return "redirect:/pos?cartError=" + encoded;
  }

  private void enrichCartModel(Model model, Cart cart) {
    model.addAttribute("cart", cart);
    String username = currentUsername();
    if (username != null) {
      model.addAttribute("holds", heldSaleRepo.findByCashierUsernameOrderByCreatedAtDesc(username));
      model.addAttribute("openShift", findOpenShift(username));
    } else {
      model.addAttribute("holds", List.of());
      model.addAttribute("openShift", null);
    }
    if (cart.getCustomerId() != null) {
      customerRepo.findById(cart.getCustomerId()).ifPresent(c -> model.addAttribute("currentCustomer", c));
    } else {
      model.addAttribute("currentCustomer", null);
    }
  }

  private Shift findOpenShift(String username) {
    if (username == null || username.isBlank()) return null;
    return shiftRepo.findByCashierUsernameAndStatus(username, ShiftStatus.OPEN).orElse(null);
  }

  private Customer loadCustomer(Long customerId) {
    if (customerId == null) return null;
    return customerRepo.findById(customerId).orElse(null);
  }

  private void addPayment(List<SalePayment> payments, PaymentMethod method, BigDecimal amount) {
    if (method == null || amount == null) return;
    if (amount.compareTo(BigDecimal.ZERO) <= 0) return;
    SalePayment payment = new SalePayment();
    payment.setMethod(method);
    payment.setAmount(amount.setScale(2, RoundingMode.HALF_UP));
    payments.add(payment);
  }

  private BigDecimal safeNetTotal(Sale sale) {
    BigDecimal total = sale.getTotal() == null ? BigDecimal.ZERO : sale.getTotal();
    BigDecimal refunded = sale.getRefundedTotal() == null ? BigDecimal.ZERO : sale.getRefundedTotal();
    return total.subtract(refunded);
  }

  @PostMapping("/checkout")
  public String checkout(@RequestParam PaymentMethod method,
                         @ModelAttribute("cart") Cart cart,
                         SessionStatus sessionStatus,
                         RedirectAttributes redirectAttributes,
                         @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                         Model model) {
    if (cart.getItems().isEmpty()) {
      if (isHtmx(hxRequest)) {
        model.addAttribute("scanError", "Cart is empty.");
        enrichCartModel(model, cart);
        return "pos/fragments :: cartContainer";
      }
      return "redirect:/pos?scanError=Cart+is+empty";
    }
    try {
      String username = currentUsername();
      Shift openShift = findOpenShift(username);
      if (openShift == null) {
        if (isHtmx(hxRequest)) {
          model.addAttribute("cartError", "Open a shift before checkout.");
          enrichCartModel(model, cart);
          return "pos/fragments :: cartContainer";
        }
        return "redirect:/pos?cartError=Open+a+shift+before+checkout.";
      }
      Customer customer = loadCustomer(cart.getCustomerId());
      Sale sale = posService.checkout(cart, method, username, customer, openShift);
      sessionStatus.setComplete(); // clears session cart
      if (isHtmx(hxRequest)) {
        Cart fresh = new Cart();
        enrichCartModel(model, fresh);
        model.addAttribute("checkoutSuccess", "Payment received. Receipt ready.");
        model.addAttribute("receiptUrl", "/sales/" + sale.getId() + "/receipt");
        return "pos/fragments :: cartContainer";
      }
      redirectAttributes.addFlashAttribute("successMessage", "Payment received. Receipt ready.");
      return "redirect:/sales/" + sale.getId() + "/receipt";
    } catch (IllegalStateException ex) {
      String message = ex.getMessage() == null ? "Checkout failed." : ex.getMessage();
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", message);
        enrichCartModel(model, cart);
        return "pos/fragments :: cartContainer";
      }
      String encoded = org.springframework.web.util.UriUtils.encode(message, java.nio.charset.StandardCharsets.UTF_8);
      return "redirect:/pos?cartError=" + encoded;
    }
  }

  @PostMapping("/checkout/split")
  public String checkoutSplit(@RequestParam(required = false) PaymentMethod method1,
                              @RequestParam(required = false) BigDecimal amount1,
                              @RequestParam(required = false) PaymentMethod method2,
                              @RequestParam(required = false) BigDecimal amount2,
                              @RequestParam(required = false) PaymentMethod method3,
                              @RequestParam(required = false) BigDecimal amount3,
                              @ModelAttribute("cart") Cart cart,
                              SessionStatus sessionStatus,
                              RedirectAttributes redirectAttributes,
                              @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                              Model model) {
    if (cart.getItems().isEmpty()) {
      if (isHtmx(hxRequest)) {
        model.addAttribute("scanError", "Cart is empty.");
        enrichCartModel(model, cart);
        return "pos/fragments :: cartContainer";
      }
      return "redirect:/pos?scanError=Cart+is+empty";
    }
    List<SalePayment> payments = new ArrayList<>();
    addPayment(payments, method1, amount1);
    addPayment(payments, method2, amount2);
    addPayment(payments, method3, amount3);
    if (payments.isEmpty()) {
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", "Enter at least one payment amount.");
        enrichCartModel(model, cart);
        return "pos/fragments :: cartContainer";
      }
      return "redirect:/pos?cartError=Enter+at+least+one+payment+amount.";
    }
    BigDecimal total = payments.stream()
            .map(SalePayment::getAmount)
            .reduce(BigDecimal.ZERO, BigDecimal::add);
    if (total.compareTo(cart.getTotal()) != 0) {
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", "Split total must equal cart total.");
        enrichCartModel(model, cart);
        return "pos/fragments :: cartContainer";
      }
      return "redirect:/pos?cartError=Split+total+must+equal+cart+total.";
    }
    try {
      String username = currentUsername();
      Shift openShift = findOpenShift(username);
      if (openShift == null) {
        if (isHtmx(hxRequest)) {
          model.addAttribute("cartError", "Open a shift before checkout.");
          enrichCartModel(model, cart);
          return "pos/fragments :: cartContainer";
        }
        return "redirect:/pos?cartError=Open+a+shift+before+checkout.";
      }
      Customer customer = loadCustomer(cart.getCustomerId());
      Sale sale = posService.checkoutSplit(cart, payments, username, customer, openShift);
      sessionStatus.setComplete();
      if (isHtmx(hxRequest)) {
        Cart fresh = new Cart();
        enrichCartModel(model, fresh);
        model.addAttribute("checkoutSuccess", "Payment received. Receipt ready.");
        model.addAttribute("receiptUrl", "/sales/" + sale.getId() + "/receipt");
        return "pos/fragments :: cartContainer";
      }
      redirectAttributes.addFlashAttribute("successMessage", "Payment received. Receipt ready.");
      return "redirect:/sales/" + sale.getId() + "/receipt";
    } catch (IllegalStateException ex) {
      String message = ex.getMessage() == null ? "Checkout failed." : ex.getMessage();
      if (isHtmx(hxRequest)) {
        model.addAttribute("cartError", message);
        enrichCartModel(model, cart);
        return "pos/fragments :: cartContainer";
      }
      String encoded = org.springframework.web.util.UriUtils.encode(message, java.nio.charset.StandardCharsets.UTF_8);
      return "redirect:/pos?cartError=" + encoded;
    }
  }

  private Page<Product> loadProductsPage(String q, Long categoryId, int page) {
    int safePage = Math.max(0, page);
    Pageable pageable = PageRequest.of(safePage, 24, Sort.by("name").ascending());
    if (categoryId != null) return productRepo.findByActiveTrueAndCategory_Id(categoryId, pageable);
    if (q != null && !q.isBlank()) return productRepo.findByActiveTrueAndNameContainingIgnoreCase(q, pageable);
    return productRepo.findByActiveTrue(pageable);
  }

  private String currentUsername() {
    Authentication auth = SecurityContextHolder.getContext().getAuthentication();
    if (auth == null || !auth.isAuthenticated()) return null;
    String name = auth.getName();
    if (name == null || "anonymousUser".equalsIgnoreCase(name)) return null;
    return name;
  }
}
