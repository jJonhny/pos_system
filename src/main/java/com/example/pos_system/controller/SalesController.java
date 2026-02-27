package com.example.pos_system.controller;

import com.example.pos_system.entity.Customer;
import com.example.pos_system.entity.PaymentMethod;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SaleItem;
import com.example.pos_system.entity.SalePayment;
import com.example.pos_system.entity.SaleStatus;
import com.example.pos_system.entity.UnitType;
import com.example.pos_system.repository.CustomerRepo;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.SaleRepo;
import com.example.pos_system.service.ReceiptPdfService;
import com.example.pos_system.service.ReceiptPaymentService;
import com.example.pos_system.service.SalesService;
import com.example.pos_system.service.I18nService;

import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.http.HttpServletResponse;

import com.openhtmltopdf.pdfboxout.PdfRendererBuilder;

import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAdjusters;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

@Controller
@RequestMapping("/sales")
public class SalesController {
  private static final Logger log = LoggerFactory.getLogger(SalesController.class);
  private static final int PAGE_SIZE = 20;
  private final SaleRepo saleRepo;
  private final ProductRepo productRepo;
  private final CustomerRepo customerRepo;
  private final ReceiptPdfService receiptPdfService;
  private final ReceiptPaymentService receiptPaymentService;
  private final SalesService salesService;
  private final I18nService i18nService;

  /**
   * Executes the SalesController operation.
   * <p>Return value: A fully initialized SalesController instance.</p>
   *
   * @param saleRepo Parameter of type {@code SaleRepo} used by this operation.
   * @param productRepo Parameter of type {@code ProductRepo} used by this operation.
   * @param customerRepo Parameter of type {@code CustomerRepo} used by this operation.
   * @param receiptPdfService Parameter of type {@code ReceiptPdfService} used by this operation.
   * @param receiptPaymentService Parameter of type {@code ReceiptPaymentService} used by this operation.
   * @param salesService Parameter of type {@code SalesService} used by this operation.
   * @param i18nService Parameter of type {@code I18nService} used by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  public SalesController(SaleRepo saleRepo, ProductRepo productRepo, CustomerRepo customerRepo,
                         ReceiptPdfService receiptPdfService,
                         ReceiptPaymentService receiptPaymentService,
                         SalesService salesService,
                         I18nService i18nService) {
    this.saleRepo = saleRepo;
    this.productRepo = productRepo;
    this.customerRepo = customerRepo;
    this.receiptPdfService = receiptPdfService;
    this.receiptPaymentService = receiptPaymentService;
    this.salesService = salesService;
    this.i18nService = i18nService;
  }

  /**
   * Executes the list operation.
   *
   * @param q Parameter of type {@code String} used by this operation.
   * @param method Parameter of type {@code PaymentMethod} used by this operation.
   * @param status Parameter of type {@code SaleStatus} used by this operation.
   * @param cashier Parameter of type {@code String} used by this operation.
   * @param customer Parameter of type {@code String} used by this operation.
   * @param minTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param maxTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param from Parameter of type {@code LocalDate} used by this operation.
   * @param to Parameter of type {@code LocalDate} used by this operation.
   * @param sort Parameter of type {@code String} used by this operation.
   * @param page Parameter of type {@code int} used by this operation.
   * @param model Parameter of type {@code Model} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the list operation.
   *
   * @param q Parameter of type {@code String} used by this operation.
   * @param method Parameter of type {@code PaymentMethod} used by this operation.
   * @param status Parameter of type {@code SaleStatus} used by this operation.
   * @param cashier Parameter of type {@code String} used by this operation.
   * @param customer Parameter of type {@code String} used by this operation.
   * @param minTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param maxTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param from Parameter of type {@code LocalDate} used by this operation.
   * @param to Parameter of type {@code LocalDate} used by this operation.
   * @param sort Parameter of type {@code String} used by this operation.
   * @param page Parameter of type {@code int} used by this operation.
   * @param model Parameter of type {@code Model} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the list operation.
   *
   * @param q Parameter of type {@code String} used by this operation.
   * @param method Parameter of type {@code PaymentMethod} used by this operation.
   * @param status Parameter of type {@code SaleStatus} used by this operation.
   * @param cashier Parameter of type {@code String} used by this operation.
   * @param customer Parameter of type {@code String} used by this operation.
   * @param minTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param maxTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param from Parameter of type {@code LocalDate} used by this operation.
   * @param to Parameter of type {@code LocalDate} used by this operation.
   * @param sort Parameter of type {@code String} used by this operation.
   * @param page Parameter of type {@code int} used by this operation.
   * @param model Parameter of type {@code Model} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  @GetMapping
  public String list(@RequestParam(required = false) String q,
                     @RequestParam(required = false) PaymentMethod method,
                     @RequestParam(required = false) SaleStatus status,
                     @RequestParam(required = false) String cashier,
                     @RequestParam(required = false) String customer,
                     @RequestParam(required = false) BigDecimal minTotal,
                     @RequestParam(required = false) BigDecimal maxTotal,
                     @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                     @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                     @RequestParam(defaultValue = "dateDesc") String sort,
                     @RequestParam(defaultValue = "0") int page,
                     Model model) {
    List<Sale> filtered = sortSales(filterSales(q, method, status, cashier, customer, minTotal, maxTotal, from, to), sort);
    int total = filtered.size();
    int totalPages = total == 0 ? 1 : (int) Math.ceil(total / (double) PAGE_SIZE);
    int maxPage = Math.max(0, totalPages - 1);
    int pageNum = Math.max(0, Math.min(page, maxPage));
    int fromIndex = pageNum * PAGE_SIZE;
    int toIndex = Math.min(fromIndex + PAGE_SIZE, total);
    List<Sale> pageItems = total == 0 ? List.of() : filtered.subList(fromIndex, toIndex);

    LocalDate today = LocalDate.now();
    LocalDate weekStart = today.with(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY));
    LocalDate monthStart = today.withDayOfMonth(1);
    boolean includeVoid = status != null;
    model.addAttribute("summaryToday", summarize(filtered, today, today, includeVoid));
    model.addAttribute("summaryWeek", summarize(filtered, weekStart, today, includeVoid));
    model.addAttribute("summaryMonth", summarize(filtered, monthStart, today, includeVoid));
    model.addAttribute("filteredSnapshot", buildSnapshot(filtered));

    model.addAttribute("sales", pageItems);
    model.addAttribute("page", pageNum);
    model.addAttribute("totalPages", totalPages);
    model.addAttribute("hasNext", pageNum < totalPages - 1);
    model.addAttribute("hasPrev", pageNum > 0);
    model.addAttribute("nextPage", pageNum + 1);
    model.addAttribute("prevPage", Math.max(0, pageNum - 1));
    model.addAttribute("q", q);
    model.addAttribute("method", method);
    model.addAttribute("status", status);
    model.addAttribute("cashier", cashier);
    model.addAttribute("customer", customer);
    model.addAttribute("minTotal", minTotal);
    model.addAttribute("maxTotal", maxTotal);
    model.addAttribute("from", from);
    model.addAttribute("to", to);
    model.addAttribute("sort", sort);
    model.addAttribute("profitById", buildProfitMap(pageItems));
    model.addAttribute("customerSummaryById", buildCustomerSummaryMap(pageItems));
    return "sales/list";
  }

  /**
   * Executes the receipt operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param print Parameter of type {@code String} used by this operation.
   * @param model Parameter of type {@code Model} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the receipt operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param print Parameter of type {@code String} used by this operation.
   * @param model Parameter of type {@code Model} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the receipt operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param print Parameter of type {@code String} used by this operation.
   * @param model Parameter of type {@code Model} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  @GetMapping("/{id}/receipt")
  public String receipt(@PathVariable Long id,
                        @RequestParam(required = false) String print,
                        Model model) {
    Sale sale = saleRepo.findByIdForReceipt(id).orElseThrow();
    List<ReceiptPaymentService.ReceiptPaymentLine> receiptPaymentLines = receiptPaymentService.buildLines(sale);
    model.addAttribute("sale", sale);
    model.addAttribute("receiptPaymentLines", receiptPaymentLines);
    model.addAttribute("cashReceivedBase", receiptPaymentService.totalCashReceivedBase(receiptPaymentLines));
    model.addAttribute("cashChangeBase", receiptPaymentService.totalCashChangeBase(receiptPaymentLines));
    model.addAttribute("profit", calculateProfit(sale));
    model.addAttribute("customerSummary", buildCustomerSummary(sale.getCustomer()));
    model.addAttribute("autoPrint", print != null);
    return "sales/receipt";
  }

  /**
   * Executes the returnForm operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param model Parameter of type {@code Model} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the returnForm operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param model Parameter of type {@code Model} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the returnForm operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param model Parameter of type {@code Model} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  @GetMapping("/{id}/return")
  public String returnForm(@PathVariable Long id, Model model) {
    Sale sale = saleRepo.findById(id).orElseThrow();
    model.addAttribute("sale", sale);
    return "sales/return";
  }

  /**
   * Executes the receiptPdf operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param response Parameter of type {@code HttpServletResponse} used by this operation.
   * @return void No value is returned; the method applies side effects to existing state.
   * @throws IOException If the operation cannot complete successfully.
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the receiptPdf operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param response Parameter of type {@code HttpServletResponse} used by this operation.
   * @return void No value is returned; the method applies side effects to existing state.
   * @throws IOException If the operation cannot complete successfully.
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the receiptPdf operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param response Parameter of type {@code HttpServletResponse} used by this operation.
   * @return void No value is returned; the method applies side effects to existing state.
   * @throws IOException If the operation cannot complete successfully.
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  @GetMapping("/{id}/receipt/pdf")
  @Transactional(readOnly = true)
  public void receiptPdf(@PathVariable Long id, HttpServletResponse response) throws IOException {
    Sale sale = saleRepo.findByIdForReceipt(id).orElseThrow();

    String html = receiptPdfService.renderReceiptPdf(sale);
    if (html == null || html.isBlank()) {
      throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, msg("sales.error.pdfEmpty"));
    }
    response.setContentType("application/pdf");
    response.setHeader("Content-Disposition", "attachment; filename=\"receipt-" + id + ".pdf\"");

    try {
      PdfRendererBuilder builder = new PdfRendererBuilder();
      builder.withHtmlContent(html, "http://localhost");
      builder.toStream(response.getOutputStream());
      builder.run();
    } catch (Exception ex) {
      log.error("PDF generation failed for sale {}", id, ex);
      String message = ex.getMessage() == null ? msg("sales.error.pdfFailed") : msg("sales.error.pdfFailedWithReason", ex.getMessage());
      throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, message);
    }
  }

  /**
   * Executes the processReturn operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param params Parameter of type {@code java.util.Map<String, String>} used by this operation.
   * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the processReturn operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param params Parameter of type {@code java.util.Map<String, String>} used by this operation.
   * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the processReturn operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param params Parameter of type {@code java.util.Map<String, String>} used by this operation.
   * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  @PostMapping("/{id}/return")
  public String processReturn(@PathVariable Long id,
                              @RequestParam java.util.Map<String, String> params,
                              RedirectAttributes redirectAttributes) {
    try {
      SalesService.ReturnOutcome outcome = salesService.processReturn(id, params);
      redirectAttributes.addFlashAttribute("successMessage",
              msg("sales.returnProcessed", "$" + outcome.refundTotal().setScale(2, java.math.RoundingMode.HALF_UP)));
      return "redirect:/sales/" + outcome.saleId() + "/receipt";
    } catch (IllegalArgumentException ex) {
      redirectAttributes.addFlashAttribute("errorMessage", ex.getMessage());
      if ("No return quantities selected.".equals(ex.getMessage())) {
        return "redirect:/sales/" + id + "/return";
      }
      return "redirect:/sales";
    }
  }

  /**
   * Executes the voidSale operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param redirect Parameter of type {@code String} used by this operation.
   * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the voidSale operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param redirect Parameter of type {@code String} used by this operation.
   * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the voidSale operation.
   *
   * @param id Parameter of type {@code Long} used by this operation.
   * @param redirect Parameter of type {@code String} used by this operation.
   * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  @PostMapping("/{id}/void")
  public String voidSale(@PathVariable Long id,
                         @RequestParam(required = false) String redirect,
                         RedirectAttributes redirectAttributes) {
    SalesService.VoidOutcome outcome = salesService.voidSale(id);
    if (!outcome.changed()) {
      redirectAttributes.addFlashAttribute("errorMessage", msg("sales.voidAlready", outcome.saleId()));
    } else {
      redirectAttributes.addFlashAttribute("successMessage", msg("sales.voidSuccess", outcome.saleId()));
    }
    return "redirect:" + (redirect == null || redirect.isBlank() ? "/sales" : redirect);
  }

  /**
   * Executes the exportCsv operation.
   *
   * @param q Parameter of type {@code String} used by this operation.
   * @param method Parameter of type {@code PaymentMethod} used by this operation.
   * @param status Parameter of type {@code SaleStatus} used by this operation.
   * @param ids Parameter of type {@code List<Long>} used by this operation.
   * @param cashier Parameter of type {@code String} used by this operation.
   * @param customer Parameter of type {@code String} used by this operation.
   * @param minTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param maxTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param from Parameter of type {@code LocalDate} used by this operation.
   * @param to Parameter of type {@code LocalDate} used by this operation.
   * @param sort Parameter of type {@code String} used by this operation.
   * @param response Parameter of type {@code HttpServletResponse} used by this operation.
   * @return void No value is returned; the method applies side effects to existing state.
   * @throws IOException If the operation cannot complete successfully.
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the exportCsv operation.
   *
   * @param q Parameter of type {@code String} used by this operation.
   * @param method Parameter of type {@code PaymentMethod} used by this operation.
   * @param status Parameter of type {@code SaleStatus} used by this operation.
   * @param ids Parameter of type {@code List<Long>} used by this operation.
   * @param cashier Parameter of type {@code String} used by this operation.
   * @param customer Parameter of type {@code String} used by this operation.
   * @param minTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param maxTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param from Parameter of type {@code LocalDate} used by this operation.
   * @param to Parameter of type {@code LocalDate} used by this operation.
   * @param sort Parameter of type {@code String} used by this operation.
   * @param response Parameter of type {@code HttpServletResponse} used by this operation.
   * @return void No value is returned; the method applies side effects to existing state.
   * @throws IOException If the operation cannot complete successfully.
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the exportCsv operation.
   *
   * @param q Parameter of type {@code String} used by this operation.
   * @param method Parameter of type {@code PaymentMethod} used by this operation.
   * @param status Parameter of type {@code SaleStatus} used by this operation.
   * @param ids Parameter of type {@code List<Long>} used by this operation.
   * @param cashier Parameter of type {@code String} used by this operation.
   * @param customer Parameter of type {@code String} used by this operation.
   * @param minTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param maxTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param from Parameter of type {@code LocalDate} used by this operation.
   * @param to Parameter of type {@code LocalDate} used by this operation.
   * @param sort Parameter of type {@code String} used by this operation.
   * @param response Parameter of type {@code HttpServletResponse} used by this operation.
   * @return void No value is returned; the method applies side effects to existing state.
   * @throws IOException If the operation cannot complete successfully.
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  @GetMapping("/export")
  public void exportCsv(@RequestParam(required = false) String q,
                        @RequestParam(required = false) PaymentMethod method,
                        @RequestParam(required = false) SaleStatus status,
                        @RequestParam(required = false) List<Long> ids,
                        @RequestParam(required = false) String cashier,
                        @RequestParam(required = false) String customer,
                        @RequestParam(required = false) BigDecimal minTotal,
                        @RequestParam(required = false) BigDecimal maxTotal,
                        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                        @RequestParam(defaultValue = "dateDesc") String sort,
                        HttpServletResponse response) throws IOException {
    List<Sale> filtered = (ids != null && !ids.isEmpty())
            ? saleRepo.findAllById(ids)
            : sortSales(filterSales(q, method, status, cashier, customer, minTotal, maxTotal, from, to), sort);

    response.setContentType("text/csv");
    response.setHeader("Content-Disposition", "attachment; filename=\"sales-export.csv\"");

    try (PrintWriter writer = response.getWriter()) {
      writer.println("ID,Date,Cashier,Payment,Status,Subtotal,Discount,Tax,Total,Items");
      DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
      for (Sale sale : filtered) {
        writer.println(buildCsvRow(sale, formatter));
      }
    }
  }

  /**
   * Executes the filterSales operation.
   *
   * @param q Parameter of type {@code String} used by this operation.
   * @param method Parameter of type {@code PaymentMethod} used by this operation.
   * @param status Parameter of type {@code SaleStatus} used by this operation.
   * @param cashier Parameter of type {@code String} used by this operation.
   * @param customer Parameter of type {@code String} used by this operation.
   * @param minTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param maxTotal Parameter of type {@code BigDecimal} used by this operation.
   * @param from Parameter of type {@code LocalDate} used by this operation.
   * @param to Parameter of type {@code LocalDate} used by this operation.
   * @return {@code List<Sale>} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private List<Sale> filterSales(String q, PaymentMethod method, SaleStatus status, String cashier, String customer,
                                 BigDecimal minTotal, BigDecimal maxTotal, LocalDate from, LocalDate to) {
    List<Sale> sales = saleRepo.findAll(Sort.by(Sort.Direction.DESC, "createdAt"));
    Stream<Sale> stream = sales.stream();
    if (q != null && !q.isBlank()) {
      String query = q.trim().toLowerCase();
      BigDecimal amountQuery = null;
      try { amountQuery = new BigDecimal(query); } catch (NumberFormatException ignored) {}
      BigDecimal finalAmountQuery = amountQuery;
      stream = stream.filter(s -> matchesQuery(s, query, finalAmountQuery));
    }
    if (method != null) stream = stream.filter(s -> matchesMethod(s, method));
    if (status != null) stream = stream.filter(s -> status.equals(s.getStatus()));
    if (cashier != null && !cashier.isBlank()) {
      String cashierQuery = cashier.trim().toLowerCase();
      stream = stream.filter(s -> s.getCashierUsername() != null &&
              s.getCashierUsername().toLowerCase().contains(cashierQuery));
    }
    if (customer != null && !customer.isBlank()) {
      String customerQuery = customer.trim().toLowerCase();
      stream = stream.filter(s -> matchesCustomer(s, customerQuery));
    }
    if (minTotal != null) stream = stream.filter(s -> safeAmount(s.getTotal()).compareTo(minTotal) >= 0);
    if (maxTotal != null) stream = stream.filter(s -> safeAmount(s.getTotal()).compareTo(maxTotal) <= 0);
    if (from != null) stream = stream.filter(s -> s.getCreatedAt() != null &&
      !s.getCreatedAt().toLocalDate().isBefore(from));
    if (to != null) stream = stream.filter(s -> s.getCreatedAt() != null &&
      !s.getCreatedAt().toLocalDate().isAfter(to));
    return stream.toList();
  }

  /**
   * Executes the sortSales operation.
   *
   * @param sales Parameter of type {@code List<Sale>} used by this operation.
   * @param sort Parameter of type {@code String} used by this operation.
   * @return {@code List<Sale>} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private List<Sale> sortSales(List<Sale> sales, String sort) {
    Comparator<Sale> createdAtAsc = Comparator.comparing(Sale::getCreatedAt,
            Comparator.nullsLast(Comparator.naturalOrder()));
    Comparator<Sale> totalAsc = Comparator.comparing(s -> safeAmount(s.getTotal()));
    Comparator<Sale> idAsc = Comparator.comparing(Sale::getId, Comparator.nullsLast(Comparator.naturalOrder()));
    Comparator<Sale> customerAsc = Comparator.comparing(
            s -> s.getCustomer() == null || s.getCustomer().getName() == null
                    ? ""
                    : s.getCustomer().getName().toLowerCase()
    );
    Comparator<Sale> cashierAsc = Comparator.comparing(
            s -> s.getCashierUsername() == null ? "" : s.getCashierUsername().toLowerCase()
    );
    Comparator<Sale> statusAsc = Comparator.comparing(
            s -> s.getStatus() == null ? "" : s.getStatus().name()
    );

    Comparator<Sale> comparator = switch (sort == null ? "" : sort) {
      case "dateAsc" -> createdAtAsc;
      case "totalDesc" -> totalAsc.reversed().thenComparing(idAsc.reversed());
      case "totalAsc" -> totalAsc.thenComparing(idAsc.reversed());
      case "idDesc" -> idAsc.reversed();
      case "idAsc" -> idAsc;
      case "cashierAsc" -> cashierAsc.thenComparing(createdAtAsc.reversed());
      case "customerAsc" -> customerAsc.thenComparing(createdAtAsc.reversed());
      case "statusAsc" -> statusAsc.thenComparing(createdAtAsc.reversed());
      default -> createdAtAsc.reversed();
    };
    return sales.stream().sorted(comparator).toList();
  }

  /**
   * Executes the buildSnapshot operation.
   *
   * @param sales Parameter of type {@code List<Sale>} used by this operation.
   * @return {@code SalesSnapshot} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private SalesSnapshot buildSnapshot(List<Sale> sales) {
    BigDecimal gross = BigDecimal.ZERO;
    BigDecimal net = BigDecimal.ZERO;
    BigDecimal refunded = BigDecimal.ZERO;
    int voidCount = 0;
    int paidCount = 0;

    for (Sale sale : sales) {
      gross = gross.add(safeAmount(sale.getTotal()));
      refunded = refunded.add(safeAmount(sale.getRefundedTotal()));
      if (sale.getStatus() == SaleStatus.VOID) {
        voidCount++;
        continue;
      }
      paidCount++;
      net = net.add(safeNetTotal(sale));
    }
    BigDecimal avgTicket = paidCount == 0
            ? BigDecimal.ZERO
            : net.divide(BigDecimal.valueOf(paidCount), 2, RoundingMode.HALF_UP);
    return new SalesSnapshot(sales.size(), gross, net, refunded, voidCount, avgTicket);
  }

  /**
   * Executes the matchesQuery operation.
   *
   * @param sale Parameter of type {@code Sale} used by this operation.
   * @param query Parameter of type {@code String} used by this operation.
   * @param amountQuery Parameter of type {@code BigDecimal} used by this operation.
   * @return {@code boolean} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private boolean matchesQuery(Sale sale, String query, BigDecimal amountQuery) {
    if (sale.getId() != null && String.valueOf(sale.getId()).contains(query)) return true;
    if (sale.getCashierUsername() != null &&
        sale.getCashierUsername().toLowerCase().contains(query)) return true;
    if (sale.getCustomer() != null) {
      if (sale.getCustomer().getName() != null &&
          sale.getCustomer().getName().toLowerCase().contains(query)) return true;
      if (sale.getCustomer().getPhone() != null &&
          sale.getCustomer().getPhone().toLowerCase().contains(query)) return true;
      if (sale.getCustomer().getEmail() != null &&
          sale.getCustomer().getEmail().toLowerCase().contains(query)) return true;
    }
    if (sale.getPaymentMethod() != null &&
        sale.getPaymentMethod().name().toLowerCase().contains(query)) return true;
    if (sale.getPayments() != null) {
      for (SalePayment payment : sale.getPayments()) {
        if (payment.getMethod() != null &&
            payment.getMethod().name().toLowerCase().contains(query)) return true;
      }
    }
    if (sale.getStatus() != null &&
        sale.getStatus().name().toLowerCase().contains(query)) return true;
    if (amountQuery != null && sale.getTotal() != null &&
        sale.getTotal().compareTo(amountQuery) == 0) return true;
    return matchesProductName(sale, query);
  }

  /**
   * Executes the matchesProductName operation.
   *
   * @param sale Parameter of type {@code Sale} used by this operation.
   * @param query Parameter of type {@code String} used by this operation.
   * @return {@code boolean} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private boolean matchesProductName(Sale sale, String query) {
    if (sale.getItems() == null) return false;
    for (SaleItem item : sale.getItems()) {
      if (item.getProduct() == null || item.getProduct().getName() == null) continue;
      if (item.getProduct().getName().toLowerCase().contains(query)) return true;
    }
    return false;
  }

  /**
   * Executes the matchesCustomer operation.
   *
   * @param sale Parameter of type {@code Sale} used by this operation.
   * @param query Parameter of type {@code String} used by this operation.
   * @return {@code boolean} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private boolean matchesCustomer(Sale sale, String query) {
    Customer customer = sale.getCustomer();
    if (customer == null) return false;
    if (customer.getName() != null && customer.getName().toLowerCase().contains(query)) return true;
    if (customer.getPhone() != null && customer.getPhone().toLowerCase().contains(query)) return true;
    if (customer.getEmail() != null && customer.getEmail().toLowerCase().contains(query)) return true;
    return false;
  }

  /**
   * Executes the summarize operation.
   *
   * @param sales Parameter of type {@code List<Sale>} used by this operation.
   * @param start Parameter of type {@code LocalDate} used by this operation.
   * @param end Parameter of type {@code LocalDate} used by this operation.
   * @param includeVoid Parameter of type {@code boolean} used by this operation.
   * @return {@code SummaryStats} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private SummaryStats summarize(List<Sale> sales, LocalDate start, LocalDate end, boolean includeVoid) {
    BigDecimal total = BigDecimal.ZERO;
    int count = 0;
    for (Sale sale : sales) {
      LocalDateTime createdAt = sale.getCreatedAt();
      if (createdAt == null) continue;
      LocalDate date = createdAt.toLocalDate();
      if (date.isBefore(start) || date.isAfter(end)) continue;
      if (!includeVoid && sale.getStatus() == SaleStatus.VOID) continue;
      count++;
      total = total.add(safeNetTotal(sale));
    }
    return new SummaryStats(total, count);
  }

  /**
   * Executes the buildCsvRow operation.
   *
   * @param sale Parameter of type {@code Sale} used by this operation.
   * @param formatter Parameter of type {@code DateTimeFormatter} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private String buildCsvRow(Sale sale, DateTimeFormatter formatter) {
    String id = sale.getId() == null ? "" : String.valueOf(sale.getId());
    String date = sale.getCreatedAt() == null ? "" : formatter.format(sale.getCreatedAt());
    String cashier = sale.getCashierUsername() == null ? "" : sale.getCashierUsername();
    String method = buildPaymentLabel(sale);
    String status = sale.getStatus() == null ? "" : sale.getStatus().name();
    String subtotal = sale.getSubtotal() == null ? "" : sale.getSubtotal().toPlainString();
    String discount = sale.getDiscount() == null ? "" : sale.getDiscount().toPlainString();
    String tax = sale.getTax() == null ? "" : sale.getTax().toPlainString();
    String total = sale.getTotal() == null ? "" : sale.getTotal().toPlainString();
    String items = buildItemsLabel(sale);

    return String.join(",",
      csv(id),
      csv(date),
      csv(cashier),
      csv(method),
      csv(status),
      csv(subtotal),
      csv(discount),
      csv(tax),
      csv(total),
      csv(items)
    );
  }

  /**
   * Executes the buildItemsLabel operation.
   *
   * @param sale Parameter of type {@code Sale} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private String buildItemsLabel(Sale sale) {
    if (sale.getItems() == null || sale.getItems().isEmpty()) return "";
    StringBuilder label = new StringBuilder();
    for (SaleItem item : sale.getItems()) {
      if (item.getProduct() == null || item.getProduct().getName() == null) continue;
      if (label.length() > 0) label.append("; ");
      int qty = item.getQty() == null ? 0 : item.getQty();
      int returned = item.getReturnedQty() == null ? 0 : item.getReturnedQty();
      int effective = Math.max(0, qty - returned);
      label.append(item.getProduct().getName())
              .append(" x")
              .append(effective)
              .append(" ")
              .append(unitLabel(item.getUnitType()));
    }
    return label.toString();
  }

  /**
   * Executes the matchesMethod operation.
   *
   * @param sale Parameter of type {@code Sale} used by this operation.
   * @param method Parameter of type {@code PaymentMethod} used by this operation.
   * @return {@code boolean} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private boolean matchesMethod(Sale sale, PaymentMethod method) {
    if (sale.getPaymentMethod() != null && sale.getPaymentMethod() == method) return true;
    if (sale.getPayments() == null) return false;
    for (SalePayment payment : sale.getPayments()) {
      if (payment.getMethod() == method) return true;
    }
    return false;
  }

  /**
   * Executes the buildPaymentLabel operation.
   *
   * @param sale Parameter of type {@code Sale} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private String buildPaymentLabel(Sale sale) {
    if (sale.getPayments() != null && !sale.getPayments().isEmpty()) {
      return "MIXED";
    }
    return sale.getPaymentMethod() == null ? "" : sale.getPaymentMethod().name();
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
    return net.compareTo(BigDecimal.ZERO) < 0 ? BigDecimal.ZERO : net;
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
   * Executes the unitSize operation.
   *
   * @param item Parameter of type {@code SaleItem} used by this operation.
   * @return {@code int} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private int unitSize(SaleItem item) {
    if (item == null) return 1;
    Integer size = item.getUnitSize();
    return size == null || size <= 0 ? 1 : size;
  }

  /**
   * Executes the unitLabel operation.
   *
   * @param unitType Parameter of type {@code UnitType} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private String unitLabel(UnitType unitType) {
    if (unitType == null) return "pc";
    return switch (unitType) {
      case BOX -> "box";
      case CASE -> "case";
      default -> "pc";
    };
  }

  /**
   * Executes the csv operation.
   *
   * @param value Parameter of type {@code String} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private String csv(String value) {
    if (value == null) return "";
    boolean needsQuotes = value.contains(",") || value.contains("\"") || value.contains("\n");
    if (!needsQuotes) return value;
    return "\"" + value.replace("\"", "\"\"") + "\"";
  }

  private static class SummaryStats {
    private final BigDecimal total;
    private final int count;

    /**
     * Executes the SummaryStats operation.
     * <p>Return value: A fully initialized SummaryStats instance.</p>
     *
     * @param total Parameter of type {@code BigDecimal} used by this operation.
     * @param count Parameter of type {@code int} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private SummaryStats(BigDecimal total, int count) {
      this.total = total;
      this.count = count;
    }

    /**
     * Executes the getTotal operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getTotal() { return total; }
    /**
     * Executes the getCount operation.
     *
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public int getCount() { return count; }
  }

  private static class SalesSnapshot {
    private final int count;
    private final BigDecimal gross;
    private final BigDecimal net;
    private final BigDecimal refunded;
    private final int voidCount;
    private final BigDecimal avgTicket;

    /**
     * Executes the SalesSnapshot operation.
     * <p>Return value: A fully initialized SalesSnapshot instance.</p>
     *
     * @param count Parameter of type {@code int} used by this operation.
     * @param gross Parameter of type {@code BigDecimal} used by this operation.
     * @param net Parameter of type {@code BigDecimal} used by this operation.
     * @param refunded Parameter of type {@code BigDecimal} used by this operation.
     * @param voidCount Parameter of type {@code int} used by this operation.
     * @param avgTicket Parameter of type {@code BigDecimal} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private SalesSnapshot(int count, BigDecimal gross, BigDecimal net, BigDecimal refunded, int voidCount, BigDecimal avgTicket) {
      this.count = count;
      this.gross = gross;
      this.net = net;
      this.refunded = refunded;
      this.voidCount = voidCount;
      this.avgTicket = avgTicket;
    }

    /**
     * Executes the getCount operation.
     *
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public int getCount() { return count; }
    /**
     * Executes the getGross operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getGross() { return gross; }
    /**
     * Executes the getNet operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getNet() { return net; }
    /**
     * Executes the getRefunded operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getRefunded() { return refunded; }
    /**
     * Executes the getVoidCount operation.
     *
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public int getVoidCount() { return voidCount; }
    /**
     * Executes the getAvgTicket operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getAvgTicket() { return avgTicket; }
  }

  /**
   * Executes the buildProfitMap operation.
   *
   * @param sales Parameter of type {@code List<Sale>} used by this operation.
   * @return {@code Map<Long, ProfitInfo>} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private Map<Long, ProfitInfo> buildProfitMap(List<Sale> sales) {
    Map<Long, ProfitInfo> map = new HashMap<>();
    for (Sale sale : sales) {
      if (sale.getId() == null) continue;
      map.put(sale.getId(), calculateProfit(sale));
    }
    return map;
  }

  /**
   * Executes the calculateProfit operation.
   *
   * @param sale Parameter of type {@code Sale} used by this operation.
   * @return {@code ProfitInfo} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private ProfitInfo calculateProfit(Sale sale) {
    BigDecimal profit = BigDecimal.ZERO;
    boolean missingCost = false;
    if (sale.getItems() != null) {
      for (SaleItem item : sale.getItems()) {
        int qty = item.getQty() == null ? 0 : item.getQty();
        int returned = item.getReturnedQty() == null ? 0 : item.getReturnedQty();
        int effectiveQty = Math.max(0, qty - returned);
        if (effectiveQty == 0) continue;
        BigDecimal unitPrice = safeAmount(item.getUnitPrice());
        BigDecimal cost = BigDecimal.ZERO;
        if (item.getProduct() == null || item.getProduct().getCostPrice() == null) {
          missingCost = true;
        } else {
          cost = item.getProduct().getCostPrice();
        }
        int unitSize = unitSize(item);
        BigDecimal costPerUnit = cost.multiply(BigDecimal.valueOf(unitSize));
        profit = profit.add(unitPrice.subtract(costPerUnit).multiply(BigDecimal.valueOf(effectiveQty)));
      }
    }
    BigDecimal netTotal = safeNetTotal(sale);
    BigDecimal margin = BigDecimal.ZERO;
    if (netTotal.compareTo(BigDecimal.ZERO) > 0) {
      margin = profit.divide(netTotal, 4, RoundingMode.HALF_UP).multiply(new BigDecimal("100"));
    }
    return new ProfitInfo(profit, margin, missingCost);
  }

  /**
   * Executes the buildCustomerSummaryMap operation.
   *
   * @param sales Parameter of type {@code List<Sale>} used by this operation.
   * @return {@code Map<Long, CustomerSummary>} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private Map<Long, CustomerSummary> buildCustomerSummaryMap(List<Sale> sales) {
    Map<Long, CustomerSummary> summaryByCustomer = new HashMap<>();
    Map<Long, CustomerSummary> summaryBySale = new HashMap<>();
    for (Sale sale : sales) {
      if (sale.getId() == null || sale.getCustomer() == null || sale.getCustomer().getId() == null) continue;
      Long customerId = sale.getCustomer().getId();
      CustomerSummary summary = summaryByCustomer.get(customerId);
      if (summary == null) {
        summary = buildCustomerSummary(sale.getCustomer());
        summaryByCustomer.put(customerId, summary);
      }
      summaryBySale.put(sale.getId(), summary);
    }
    return summaryBySale;
  }

  /**
   * Executes the buildCustomerSummary operation.
   *
   * @param customer Parameter of type {@code Customer} used by this operation.
   * @return {@code CustomerSummary} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  private CustomerSummary buildCustomerSummary(Customer customer) {
    if (customer == null || customer.getId() == null) return null;
    List<Sale> sales = saleRepo.findByCustomer_Id(customer.getId());
    BigDecimal spend = BigDecimal.ZERO;
    int visits = 0;
    LocalDateTime lastPurchase = null;
    for (Sale sale : sales) {
      if (sale.getStatus() == SaleStatus.VOID) continue;
      spend = spend.add(safeNetTotal(sale));
      visits++;
      if (sale.getCreatedAt() != null && (lastPurchase == null || sale.getCreatedAt().isAfter(lastPurchase))) {
        lastPurchase = sale.getCreatedAt();
      }
    }
    return new CustomerSummary(customer, spend, visits, lastPurchase);
  }

  /**
   * Executes the bulkAction operation.
   *
   * @param action Parameter of type {@code String} used by this operation.
   * @param ids Parameter of type {@code List<Long>} used by this operation.
   * @param redirect Parameter of type {@code String} used by this operation.
   * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the bulkAction operation.
   *
   * @param action Parameter of type {@code String} used by this operation.
   * @param ids Parameter of type {@code List<Long>} used by this operation.
   * @param redirect Parameter of type {@code String} used by this operation.
   * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  /**
   * Executes the bulkAction operation.
   *
   * @param action Parameter of type {@code String} used by this operation.
   * @param ids Parameter of type {@code List<Long>} used by this operation.
   * @param redirect Parameter of type {@code String} used by this operation.
   * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
   * @return {@code String} Result produced by this operation.
   * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
   * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
   */
  @PostMapping("/bulk")
  public String bulkAction(@RequestParam String action,
                           @RequestParam(required = false) List<Long> ids,
                           @RequestParam(required = false) String redirect,
                           RedirectAttributes redirectAttributes) {
    if (ids == null || ids.isEmpty()) {
      redirectAttributes.addFlashAttribute("errorMessage", msg("sales.error.noSalesSelected"));
      return "redirect:" + (redirect == null || redirect.isBlank() ? "/sales" : redirect);
    }
    if ("void".equalsIgnoreCase(action)) {
      int voided = 0;
      int skipped = 0;
      List<Sale> sales = saleRepo.findAllById(ids);
      for (Sale sale : sales) {
        if (sale.getStatus() == SaleStatus.VOID ||
            sale.getStatus() == SaleStatus.RETURNED ||
            sale.getStatus() == SaleStatus.PARTIALLY_RETURNED) {
          skipped++;
          continue;
        }
        sale.setStatus(SaleStatus.VOID);
        saleRepo.save(sale);
        voided++;
      }
      if (voided > 0) {
        redirectAttributes.addFlashAttribute("successMessage", msg("sales.bulkVoided", voided));
      }
      if (skipped > 0) {
        redirectAttributes.addFlashAttribute("errorMessage", msg("sales.bulkSkipped", skipped));
      }
    } else {
      redirectAttributes.addFlashAttribute("errorMessage", msg("sales.error.unknownBulkAction"));
    }
    return "redirect:" + (redirect == null || redirect.isBlank() ? "/sales" : redirect);
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

  private static class ProfitInfo {
    private final BigDecimal profit;
    private final BigDecimal marginPct;
    private final boolean missingCost;

    /**
     * Executes the ProfitInfo operation.
     * <p>Return value: A fully initialized ProfitInfo instance.</p>
     *
     * @param profit Parameter of type {@code BigDecimal} used by this operation.
     * @param marginPct Parameter of type {@code BigDecimal} used by this operation.
     * @param missingCost Parameter of type {@code boolean} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private ProfitInfo(BigDecimal profit, BigDecimal marginPct, boolean missingCost) {
      this.profit = profit;
      this.marginPct = marginPct;
      this.missingCost = missingCost;
    }

    /**
     * Executes the getProfit operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getProfit() { return profit; }
    /**
     * Executes the getMarginPct operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getMarginPct() { return marginPct; }
    /**
     * Executes the isMissingCost operation.
     *
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public boolean isMissingCost() { return missingCost; }
  }

  private static class CustomerSummary {
    private final Customer customer;
    private final BigDecimal lifetimeSpend;
    private final int visits;
    private final LocalDateTime lastPurchase;

    /**
     * Executes the CustomerSummary operation.
     * <p>Return value: A fully initialized CustomerSummary instance.</p>
     *
     * @param customer Parameter of type {@code Customer} used by this operation.
     * @param lifetimeSpend Parameter of type {@code BigDecimal} used by this operation.
     * @param visits Parameter of type {@code int} used by this operation.
     * @param lastPurchase Parameter of type {@code LocalDateTime} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private CustomerSummary(Customer customer, BigDecimal lifetimeSpend, int visits, LocalDateTime lastPurchase) {
      this.customer = customer;
      this.lifetimeSpend = lifetimeSpend;
      this.visits = visits;
      this.lastPurchase = lastPurchase;
    }

    /**
     * Executes the getCustomer operation.
     *
     * @return {@code Customer} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Customer getCustomer() { return customer; }
    /**
     * Executes the getLifetimeSpend operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getLifetimeSpend() { return lifetimeSpend; }
    /**
     * Executes the getVisits operation.
     *
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public int getVisits() { return visits; }
    /**
     * Executes the getLastPurchase operation.
     *
     * @return {@code LocalDateTime} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public LocalDateTime getLastPurchase() { return lastPurchase; }
  }
}
