package com.example.pos_system.controller;

import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SaleItem;
import com.example.pos_system.entity.Shift;
import com.example.pos_system.entity.ShiftStatus;
import com.example.pos_system.repository.SaleRepo;
import com.example.pos_system.repository.ShiftRepo;
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

    public ReportsController(SaleRepo saleRepo, ShiftRepo shiftRepo) {
        this.saleRepo = saleRepo;
        this.shiftRepo = shiftRepo;
    }

    @GetMapping
    public String reports(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                          @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                          Model model) {
        List<Sale> sales = filterSales(from, to);
        BigDecimal totalRevenue = sales.stream()
                .map(Sale::getTotal)
                .map(this::safeAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        BigDecimal avgTicket = sales.isEmpty()
                ? BigDecimal.ZERO
                : totalRevenue.divide(BigDecimal.valueOf(sales.size()), 2, java.math.RoundingMode.HALF_UP);

        model.addAttribute("from", from);
        model.addAttribute("to", to);
        model.addAttribute("salesCount", sales.size());
        model.addAttribute("totalRevenue", totalRevenue);
        model.addAttribute("avgTicket", avgTicket);
        return "reports/index";
    }

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

    @GetMapping("/shifts.xlsx")
    @Transactional(readOnly = true)
    public void exportShiftsExcel(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                                  @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                                  HttpServletResponse response) throws IOException {
        List<Shift> shifts = filterShifts(from, to);

        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=\"shift-summary.xlsx\"");

        try (Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("Shifts");
            Row header = sheet.createRow(0);
            String[] headers = new String[] {
                    "Shift ID", "Cashier", "Terminal", "Opened At", "Closed At", "Status",
                    "Opening Cash (Base)", "Cash In (Base)", "Cash Out (Base)",
                    "Cash Sales (Base)", "Card Sales (Base)", "QR Sales (Base)", "Total Sales (Base)",
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
                row.createCell(10).setCellValue(safeAmount(shift.getCardTotal()).doubleValue());
                row.createCell(11).setCellValue(safeAmount(shift.getQrTotal()).doubleValue());
                row.createCell(12).setCellValue(safeAmount(shift.getTotalSales()).doubleValue());
                row.createCell(13).setCellValue(safeAmount(shift.getExpectedCash()).doubleValue());
                row.createCell(14).setCellValue(safeAmount(shift.getClosingCash()).doubleValue());
                row.createCell(15).setCellValue(safeAmount(shift.getVarianceCash()).doubleValue());
                row.createCell(16).setCellValue(nullToEmpty(shift.getCloseNotes()));
                row.createCell(17).setCellValue(compactJson(shift.getOpeningFloatJson()));
                row.createCell(18).setCellValue(compactJson(shift.getExpectedAmountsJson()));
                row.createCell(19).setCellValue(compactJson(shift.getCountedAmountsJson()));
                row.createCell(20).setCellValue(compactJson(shift.getVarianceAmountsJson()));
            }

            for (int i = 0; i < headers.length; i++) {
                sheet.autoSizeColumn(i);
            }

            workbook.write(response.getOutputStream());
        }
    }

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

    private List<Shift> filterShifts(LocalDate from, LocalDate to) {
        List<Shift> shifts = shiftRepo.findAll(Sort.by(Sort.Direction.DESC, "openedAt"));
        List<Shift> filtered = new ArrayList<>();
        for (Shift shift : shifts) {
            if (shift.getStatus() != ShiftStatus.CLOSED) continue;
            LocalDate date = referenceDate(shift);
            if (date == null) continue;
            if (from != null && date.isBefore(from)) continue;
            if (to != null && date.isAfter(to)) continue;
            filtered.add(shift);
        }
        return filtered;
    }

    private LocalDate referenceDate(Shift shift) {
        if (shift == null) return null;
        LocalDateTime closedAt = shift.getClosedAt();
        if (closedAt != null) return closedAt.toLocalDate();
        LocalDateTime openedAt = shift.getOpenedAt();
        if (openedAt != null) return openedAt.toLocalDate();
        return null;
    }

    private String formatDateTime(LocalDateTime value, DateTimeFormatter formatter) {
        if (value == null) return "";
        return formatter.format(value);
    }

    private String compactJson(String value) {
        if (value == null || value.isBlank()) return "";
        return value.replaceAll("\\s+", " ").trim();
    }

    private BigDecimal safeAmount(BigDecimal amount) {
        return amount == null ? BigDecimal.ZERO : amount;
    }

    private String nullToEmpty(String value) {
        return value == null ? "" : value;
    }

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
