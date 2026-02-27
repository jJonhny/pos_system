package com.example.pos_system.controller;

import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SaleItem;
import com.example.pos_system.entity.Shift;
import com.example.pos_system.entity.ShiftStatus;
import com.example.pos_system.entity.StockMovement;
import com.example.pos_system.entity.StockMovementType;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.SaleRepo;
import com.example.pos_system.repository.ShiftRepo;
import com.example.pos_system.service.PurchaseService;
import com.example.pos_system.service.StockMovementService;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.stereotype.Controller;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.StringJoiner;
import java.util.stream.Stream;

@Controller
@RequestMapping("/reports")
public class ReportsController {
    private final SaleRepo saleRepo;
    private final ShiftRepo shiftRepo;
    private final ProductRepo productRepo;
    private final StockMovementService stockMovementService;
    private final PurchaseService purchaseService;

    /**
     * Executes the ReportsController operation.
     * <p>Return value: A fully initialized ReportsController instance.</p>
     *
     * @param saleRepo Parameter of type {@code SaleRepo} used by this operation.
     * @param shiftRepo Parameter of type {@code ShiftRepo} used by this operation.
     * @param productRepo Parameter of type {@code ProductRepo} used by this operation.
     * @param stockMovementService Parameter of type {@code StockMovementService} used by this operation.
     * @param purchaseService Parameter of type {@code PurchaseService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ReportsController(SaleRepo saleRepo,
                             ShiftRepo shiftRepo,
                             ProductRepo productRepo,
                             StockMovementService stockMovementService,
                             PurchaseService purchaseService) {
        this.saleRepo = saleRepo;
        this.shiftRepo = shiftRepo;
        this.productRepo = productRepo;
        this.stockMovementService = stockMovementService;
        this.purchaseService = purchaseService;
    }

    /**
     * Executes the reports operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param cashier Parameter of type {@code String} used by this operation.
     * @param terminal Parameter of type {@code String} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the reports operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param cashier Parameter of type {@code String} used by this operation.
     * @param terminal Parameter of type {@code String} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the reports operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param cashier Parameter of type {@code String} used by this operation.
     * @param terminal Parameter of type {@code String} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping
    public String reports(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                          @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                          @RequestParam(required = false) String cashier,
                          @RequestParam(required = false) String terminal,
                          Model model) {
        List<Sale> sales = filterSales(from, to);
        List<Shift> closedShifts = shiftRepo.findByStatusOrderByOpenedAtDesc(ShiftStatus.CLOSED);
        List<Shift> shiftSummary = filterShifts(from, to, cashier, terminal);
        BigDecimal totalRevenue = sales.stream()
                .map(Sale::getTotal)
                .map(this::safeAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        BigDecimal avgTicket = sales.isEmpty()
                ? BigDecimal.ZERO
                : totalRevenue.divide(BigDecimal.valueOf(sales.size()), 2, java.math.RoundingMode.HALF_UP);

        model.addAttribute("from", from);
        model.addAttribute("to", to);
        model.addAttribute("cashier", normalizeFilter(cashier));
        model.addAttribute("terminal", normalizeFilter(terminal));
        model.addAttribute("shiftCashiers", closedShifts.stream()
                .map(Shift::getCashierUsername)
                .map(this::normalizeFilter)
                .filter(v -> v != null && !v.isBlank())
                .distinct()
                .toList());
        model.addAttribute("shiftTerminals", closedShifts.stream()
                .map(Shift::getTerminalId)
                .map(this::normalizeFilter)
                .filter(v -> v != null && !v.isBlank())
                .distinct()
                .toList());
        model.addAttribute("shiftSummary", shiftSummary);
        model.addAttribute("salesCount", sales.size());
        model.addAttribute("totalRevenue", totalRevenue);
        model.addAttribute("avgTicket", avgTicket);
        return "reports/index";
    }

    /**
     * Executes the exportSalesExcel operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the exportSalesExcel operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the exportSalesExcel operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/sales.xlsx")
    @Transactional(readOnly = true)
    public void exportSalesExcel(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                                 @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                                 HttpServletResponse response) throws IOException {
        List<Sale> sales = filterSales(from, to);

        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=\"sales-report.xlsx\"");

        try (Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("Sales");
            Row header = sheet.createRow(0);
            String[] headers = new String[] {
                    "ID", "Date", "Cashier", "Payment", "Status",
                    "Subtotal", "Discount", "Tax", "Total", "Items"
            };
            CellStyle headerStyle = workbook.createCellStyle();
            Font headerFont = workbook.createFont();
            headerFont.setBold(true);
            headerStyle.setFont(headerFont);

            for (int i = 0; i < headers.length; i++) {
                var cell = header.createCell(i);
                cell.setCellValue(headers[i]);
                cell.setCellStyle(headerStyle);
            }

            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
            int rowIdx = 1;
            for (Sale sale : sales) {
                Row row = sheet.createRow(rowIdx++);
                row.createCell(0).setCellValue(sale.getId() == null ? "" : String.valueOf(sale.getId()));
                row.createCell(1).setCellValue(sale.getCreatedAt() == null ? "" : formatter.format(sale.getCreatedAt()));
                row.createCell(2).setCellValue(nullToEmpty(sale.getCashierUsername()));
                row.createCell(3).setCellValue(sale.getPaymentMethod() == null ? "" : sale.getPaymentMethod().name());
                row.createCell(4).setCellValue(sale.getStatus() == null ? "" : sale.getStatus().name());
                row.createCell(5).setCellValue(safeAmount(sale.getSubtotal()).doubleValue());
                row.createCell(6).setCellValue(safeAmount(sale.getDiscount()).doubleValue());
                row.createCell(7).setCellValue(safeAmount(sale.getTax()).doubleValue());
                row.createCell(8).setCellValue(safeAmount(sale.getTotal()).doubleValue());
                row.createCell(9).setCellValue(buildItemsSummary(sale));
            }

            for (int i = 0; i < headers.length; i++) {
                sheet.autoSizeColumn(i);
            }

            workbook.write(response.getOutputStream());
        }
    }

    /**
     * Executes the exportShiftsExcel operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param cashier Parameter of type {@code String} used by this operation.
     * @param terminal Parameter of type {@code String} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the exportShiftsExcel operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param cashier Parameter of type {@code String} used by this operation.
     * @param terminal Parameter of type {@code String} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the exportShiftsExcel operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param cashier Parameter of type {@code String} used by this operation.
     * @param terminal Parameter of type {@code String} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/shifts.xlsx")
    @Transactional(readOnly = true)
    public void exportShiftsExcel(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                                  @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                                  @RequestParam(required = false) String cashier,
                                  @RequestParam(required = false) String terminal,
                                  HttpServletResponse response) throws IOException {
        List<Shift> shifts = filterShifts(from, to, cashier, terminal);

        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=\"shift-summary.xlsx\"");

        try (Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("Shifts");
            Row header = sheet.createRow(0);
            String[] headers = new String[] {
                    "Shift ID", "Cashier", "Terminal", "Opened At", "Closed At", "Status",
                    "Opening Cash (Base)", "Cash In (Base)", "Cash Out (Base)",
                    "Cash Sales (Base)", "Cash Refunds (Base)", "Card Sales (Base)", "QR Sales (Base)", "Total Sales (Base)",
                    "Expected Cash (Base)", "Counted Cash (Base)", "Variance (Base)",
                    "Notes", "Opening Float (JSON)", "Expected Amounts (JSON)",
                    "Counted Amounts (JSON)", "Variance Amounts (JSON)"
            };
            CellStyle headerStyle = workbook.createCellStyle();
            Font headerFont = workbook.createFont();
            headerFont.setBold(true);
            headerStyle.setFont(headerFont);
            for (int i = 0; i < headers.length; i++) {
                var cell = header.createCell(i);
                cell.setCellValue(headers[i]);
                cell.setCellStyle(headerStyle);
            }

            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
            int rowIdx = 1;
            for (Shift shift : shifts) {
                Row row = sheet.createRow(rowIdx++);
                row.createCell(0).setCellValue(shift.getId() == null ? "" : String.valueOf(shift.getId()));
                row.createCell(1).setCellValue(nullToEmpty(shift.getCashierUsername()));
                row.createCell(2).setCellValue(nullToEmpty(shift.getTerminalId()));
                row.createCell(3).setCellValue(formatDateTime(shift.getOpenedAt(), formatter));
                row.createCell(4).setCellValue(formatDateTime(shift.getClosedAt(), formatter));
                row.createCell(5).setCellValue(shift.getStatus() == null ? "" : shift.getStatus().name());
                row.createCell(6).setCellValue(safeAmount(shift.getOpeningCash()).doubleValue());
                row.createCell(7).setCellValue(safeAmount(shift.getCashInTotal()).doubleValue());
                row.createCell(8).setCellValue(safeAmount(shift.getCashOutTotal()).doubleValue());
                row.createCell(9).setCellValue(safeAmount(shift.getCashTotal()).doubleValue());
                row.createCell(10).setCellValue(safeAmount(shift.getCashRefundTotal()).doubleValue());
                row.createCell(11).setCellValue(safeAmount(shift.getCardTotal()).doubleValue());
                row.createCell(12).setCellValue(safeAmount(shift.getQrTotal()).doubleValue());
                row.createCell(13).setCellValue(safeAmount(shift.getTotalSales()).doubleValue());
                row.createCell(14).setCellValue(safeAmount(shift.getExpectedCash()).doubleValue());
                row.createCell(15).setCellValue(safeAmount(shift.getClosingCash()).doubleValue());
                row.createCell(16).setCellValue(safeAmount(shift.getVarianceCash()).doubleValue());
                row.createCell(17).setCellValue(nullToEmpty(shift.getCloseNotes()));
                row.createCell(18).setCellValue(compactJson(shift.getOpeningFloatJson()));
                row.createCell(19).setCellValue(compactJson(shift.getExpectedAmountsJson()));
                row.createCell(20).setCellValue(compactJson(shift.getCountedAmountsJson()));
                row.createCell(21).setCellValue(compactJson(shift.getVarianceAmountsJson()));
            }

            for (int i = 0; i < headers.length; i++) {
                sheet.autoSizeColumn(i);
            }

            workbook.write(response.getOutputStream());
        }
    }

    /**
     * Executes the inventoryLedger operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the inventoryLedger operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the inventoryLedger operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/inventory-ledger")
    public String inventoryLedger(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                                  @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                                  @RequestParam(required = false) Long productId,
                                  @RequestParam(required = false) StockMovementType type,
                                  Model model) {
        List<StockMovement> rows = stockMovementService.findMovements(from, to, productId, type);
        model.addAttribute("rows", rows);
        model.addAttribute("products", productRepo.findAll(Sort.by("name").ascending()));
        model.addAttribute("types", StockMovementType.values());
        model.addAttribute("from", from);
        model.addAttribute("to", to);
        model.addAttribute("productId", productId);
        model.addAttribute("type", type);
        model.addAttribute("totalDelta", rows.stream().map(StockMovement::getQtyDelta).mapToInt(v -> v == null ? 0 : v).sum());
        return "reports/inventory-ledger";
    }

    /**
     * Executes the exportInventoryLedgerExcel operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the exportInventoryLedgerExcel operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the exportInventoryLedgerExcel operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/inventory-ledger.xlsx")
    @Transactional(readOnly = true)
    public void exportInventoryLedgerExcel(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                                           @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                                           @RequestParam(required = false) Long productId,
                                           @RequestParam(required = false) StockMovementType type,
                                           HttpServletResponse response) throws IOException {
        List<StockMovement> rows = stockMovementService.findMovements(from, to, productId, type);
        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=\"inventory-ledger.xlsx\"");

        try (Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("Inventory Ledger");
            Row header = sheet.createRow(0);
            String[] headers = {"ID", "Date", "Product", "SKU", "Type", "Qty Delta", "Unit Cost", "Currency", "Ref Type", "Ref Id", "Terminal", "Notes"};
            CellStyle headerStyle = workbook.createCellStyle();
            Font headerFont = workbook.createFont();
            headerFont.setBold(true);
            headerStyle.setFont(headerFont);
            for (int i = 0; i < headers.length; i++) {
                var cell = header.createCell(i);
                cell.setCellValue(headers[i]);
                cell.setCellStyle(headerStyle);
            }

            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
            int rowIdx = 1;
            for (StockMovement row : rows) {
                Row data = sheet.createRow(rowIdx++);
                data.createCell(0).setCellValue(row.getId() == null ? "" : String.valueOf(row.getId()));
                data.createCell(1).setCellValue(row.getCreatedAt() == null ? "" : formatter.format(row.getCreatedAt()));
                data.createCell(2).setCellValue(row.getProduct() == null ? "" : nullToEmpty(row.getProduct().getName()));
                data.createCell(3).setCellValue(row.getProduct() == null ? "" : nullToEmpty(row.getProduct().getSku()));
                data.createCell(4).setCellValue(row.getType() == null ? "" : row.getType().name());
                data.createCell(5).setCellValue(row.getQtyDelta() == null ? 0 : row.getQtyDelta());
                data.createCell(6).setCellValue(row.getUnitCost() == null ? 0 : row.getUnitCost().doubleValue());
                data.createCell(7).setCellValue(nullToEmpty(row.getCurrency()));
                data.createCell(8).setCellValue(nullToEmpty(row.getRefType()));
                data.createCell(9).setCellValue(nullToEmpty(row.getRefId()));
                data.createCell(10).setCellValue(nullToEmpty(row.getTerminalId()));
                data.createCell(11).setCellValue(nullToEmpty(row.getNotes()));
            }
            for (int i = 0; i < headers.length; i++) {
                sheet.autoSizeColumn(i);
            }
            workbook.write(response.getOutputStream());
        }
    }

    /**
     * Executes the receivingReport operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the receivingReport operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the receivingReport operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/receiving")
    public String receivingReport(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                                  @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                                  @RequestParam(required = false) Long supplierId,
                                  Model model) {
        var rows = purchaseService.buildReceivingReport(from, to, supplierId);
        model.addAttribute("rows", rows);
        model.addAttribute("suppliers", purchaseService.listSuppliers());
        model.addAttribute("from", from);
        model.addAttribute("to", to);
        model.addAttribute("supplierId", supplierId);
        model.addAttribute("totalCost", rows.stream().map(PurchaseService.ReceivingReportRow::totalCost).reduce(BigDecimal.ZERO, BigDecimal::add));
        model.addAttribute("totalQty", rows.stream().mapToInt(PurchaseService.ReceivingReportRow::totalQty).sum());
        return "reports/receiving";
    }

    /**
     * Executes the exportReceivingExcel operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the exportReceivingExcel operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the exportReceivingExcel operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/receiving.xlsx")
    @Transactional(readOnly = true)
    public void exportReceivingExcel(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                                     @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                                     @RequestParam(required = false) Long supplierId,
                                     HttpServletResponse response) throws IOException {
        var rows = purchaseService.buildReceivingReport(from, to, supplierId);
        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=\"receiving-report.xlsx\"");

        try (Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("Receiving");
            Row header = sheet.createRow(0);
            String[] headers = {"Date", "Supplier", "Receipt Count", "Total Qty", "Total Cost"};
            CellStyle headerStyle = workbook.createCellStyle();
            Font headerFont = workbook.createFont();
            headerFont.setBold(true);
            headerStyle.setFont(headerFont);
            for (int i = 0; i < headers.length; i++) {
                var cell = header.createCell(i);
                cell.setCellValue(headers[i]);
                cell.setCellStyle(headerStyle);
            }

            int rowIdx = 1;
            for (PurchaseService.ReceivingReportRow row : rows) {
                Row data = sheet.createRow(rowIdx++);
                data.createCell(0).setCellValue(row.date() == null ? "" : row.date().toString());
                data.createCell(1).setCellValue(nullToEmpty(row.supplierName()));
                data.createCell(2).setCellValue(row.receiptCount());
                data.createCell(3).setCellValue(row.totalQty());
                data.createCell(4).setCellValue(row.totalCost() == null ? 0 : row.totalCost().doubleValue());
            }
            for (int i = 0; i < headers.length; i++) {
                sheet.autoSizeColumn(i);
            }
            workbook.write(response.getOutputStream());
        }
    }

    /**
     * Executes the filterSales operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @return {@code List<Sale>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<Sale> filterSales(LocalDate from, LocalDate to) {
        List<Sale> sales = saleRepo.findAll(Sort.by(Sort.Direction.DESC, "createdAt"));
        Stream<Sale> stream = sales.stream();
        if (from != null) {
            stream = stream.filter(sale -> sale.getCreatedAt() != null &&
                    !sale.getCreatedAt().toLocalDate().isBefore(from));
        }
        if (to != null) {
            stream = stream.filter(sale -> sale.getCreatedAt() != null &&
                    !sale.getCreatedAt().toLocalDate().isAfter(to));
        }
        return stream.toList();
    }

    /**
     * Executes the filterShifts operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param cashier Parameter of type {@code String} used by this operation.
     * @param terminal Parameter of type {@code String} used by this operation.
     * @return {@code List<Shift>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<Shift> filterShifts(LocalDate from, LocalDate to, String cashier, String terminal) {
        List<Shift> shifts = shiftRepo.findAll(Sort.by(Sort.Direction.DESC, "openedAt"));
        String cashierFilter = normalizeFilter(cashier);
        String terminalFilter = normalizeFilter(terminal);
        List<Shift> filtered = new ArrayList<>();
        for (Shift shift : shifts) {
            if (shift.getStatus() != ShiftStatus.CLOSED) continue;
            if (cashierFilter != null) {
                String value = normalizeFilter(shift.getCashierUsername());
                if (value == null || !value.equalsIgnoreCase(cashierFilter)) continue;
            }
            if (terminalFilter != null) {
                String value = normalizeFilter(shift.getTerminalId());
                if (value == null || !value.equalsIgnoreCase(terminalFilter)) continue;
            }
            LocalDate date = referenceDate(shift);
            if (date == null) continue;
            if (from != null && date.isBefore(from)) continue;
            if (to != null && date.isAfter(to)) continue;
            filtered.add(shift);
        }
        return filtered;
    }

    /**
     * Executes the normalizeFilter operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalizeFilter(String value) {
        if (value == null) return null;
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    /**
     * Executes the referenceDate operation.
     *
     * @param shift Parameter of type {@code Shift} used by this operation.
     * @return {@code LocalDate} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private LocalDate referenceDate(Shift shift) {
        if (shift == null) return null;
        LocalDateTime closedAt = shift.getClosedAt();
        if (closedAt != null) return closedAt.toLocalDate();
        LocalDateTime openedAt = shift.getOpenedAt();
        if (openedAt != null) return openedAt.toLocalDate();
        return null;
    }

    /**
     * Executes the formatDateTime operation.
     *
     * @param value Parameter of type {@code LocalDateTime} used by this operation.
     * @param formatter Parameter of type {@code DateTimeFormatter} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String formatDateTime(LocalDateTime value, DateTimeFormatter formatter) {
        if (value == null) return "";
        return formatter.format(value);
    }

    /**
     * Executes the compactJson operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String compactJson(String value) {
        if (value == null || value.isBlank()) return "";
        return value.replaceAll("\\s+", " ").trim();
    }

    /**
     * Executes the safeAmount operation.
     *
     * @param amount Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeAmount(BigDecimal amount) {
        return amount == null ? BigDecimal.ZERO : amount;
    }

    /**
     * Executes the nullToEmpty operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String nullToEmpty(String value) {
        return value == null ? "" : value;
    }

    /**
     * Executes the buildItemsSummary operation.
     *
     * @param sale Parameter of type {@code Sale} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String buildItemsSummary(Sale sale) {
        if (sale.getItems() == null || sale.getItems().isEmpty()) return "";
        StringJoiner joiner = new StringJoiner(" | ");
        for (SaleItem item : sale.getItems()) {
            String name = item.getProduct() != null && item.getProduct().getName() != null
                    ? item.getProduct().getName()
                    : "Item";
            int qty = item.getQty() == null ? 0 : item.getQty();
            joiner.add(name + " x" + qty);
        }
        return joiner.toString();
    }
}
