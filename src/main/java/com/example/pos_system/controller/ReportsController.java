package com.example.pos_system.controller;

import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SaleItem;
import com.example.pos_system.repository.SaleRepo;
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
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.StringJoiner;
import java.util.stream.Stream;

@Controller
@RequestMapping("/reports")
public class ReportsController {
    private final SaleRepo saleRepo;

    public ReportsController(SaleRepo saleRepo) {
        this.saleRepo = saleRepo;
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
