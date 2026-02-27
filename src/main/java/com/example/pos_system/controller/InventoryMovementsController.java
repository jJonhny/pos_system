package com.example.pos_system.controller;

import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.StockMovement;
import com.example.pos_system.entity.StockMovementType;
import com.example.pos_system.repository.ProductRepo;
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
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;

@Controller
@RequestMapping("/inventory/movements")
public class InventoryMovementsController {
    private final StockMovementService stockMovementService;
    private final ProductRepo productRepo;

    /**
     * Executes the InventoryMovementsController operation.
     * <p>Return value: A fully initialized InventoryMovementsController instance.</p>
     *
     * @param stockMovementService Parameter of type {@code StockMovementService} used by this operation.
     * @param productRepo Parameter of type {@code ProductRepo} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public InventoryMovementsController(StockMovementService stockMovementService, ProductRepo productRepo) {
        this.stockMovementService = stockMovementService;
        this.productRepo = productRepo;
    }

    /**
     * Executes the list operation.
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
     * Executes the list operation.
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
     * Executes the list operation.
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
    @GetMapping
    public String list(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                       @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                       @RequestParam(required = false) Long productId,
                       @RequestParam(required = false) StockMovementType type,
                       Model model) {
        List<StockMovement> movements = stockMovementService.findMovements(from, to, productId, type);
        model.addAttribute("movements", movements);
        model.addAttribute("products", productRepo.findAll(Sort.by("name").ascending()));
        model.addAttribute("types", StockMovementType.values());
        model.addAttribute("from", from);
        model.addAttribute("to", to);
        model.addAttribute("productId", productId);
        model.addAttribute("type", type);
        model.addAttribute("totalDelta", movements.stream().map(StockMovement::getQtyDelta).mapToInt(i -> i == null ? 0 : i).sum());
        return "inventory/movements";
    }

    /**
     * Executes the exportCsv operation.
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
     * Executes the exportCsv operation.
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
     * Executes the exportCsv operation.
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
    @GetMapping("/export.csv")
    public void exportCsv(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                          @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                          @RequestParam(required = false) Long productId,
                          @RequestParam(required = false) StockMovementType type,
                          HttpServletResponse response) throws IOException {
        List<StockMovement> rows = stockMovementService.findMovements(from, to, productId, type);
        response.setContentType("text/csv");
        response.setHeader("Content-Disposition", "attachment; filename=\"inventory-movements.csv\"");
        try (PrintWriter writer = response.getWriter()) {
            writer.println("ID,Created At,Product,SKU,Type,Qty Delta,Unit Cost,Currency,Ref Type,Ref Id,Terminal,Notes");
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
            for (StockMovement row : rows) {
                Product product = row.getProduct();
                writer.println(String.join(",",
                        csv(stringValue(row.getId())),
                        csv(row.getCreatedAt() == null ? "" : formatter.format(row.getCreatedAt())),
                        csv(product == null ? "" : nullToEmpty(product.getName())),
                        csv(product == null ? "" : nullToEmpty(product.getSku())),
                        csv(row.getType() == null ? "" : row.getType().name()),
                        csv(row.getQtyDelta() == null ? "" : String.valueOf(row.getQtyDelta())),
                        csv(row.getUnitCost() == null ? "" : row.getUnitCost().toPlainString()),
                        csv(nullToEmpty(row.getCurrency())),
                        csv(nullToEmpty(row.getRefType())),
                        csv(nullToEmpty(row.getRefId())),
                        csv(nullToEmpty(row.getTerminalId())),
                        csv(nullToEmpty(row.getNotes()))
                ));
            }
        }
    }

    /**
     * Executes the exportExcel operation.
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
     * Executes the exportExcel operation.
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
     * Executes the exportExcel operation.
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
    @GetMapping("/export.xlsx")
    public void exportExcel(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                            @RequestParam(required = false) Long productId,
                            @RequestParam(required = false) StockMovementType type,
                            HttpServletResponse response) throws IOException {
        List<StockMovement> rows = stockMovementService.findMovements(from, to, productId, type);
        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=\"inventory-movements.xlsx\"");

        try (Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("Movements");
            String[] headers = {"ID", "Created At", "Product", "SKU", "Type", "Qty Delta", "Unit Cost", "Currency", "Ref Type", "Ref Id", "Terminal", "Notes"};

            CellStyle headerStyle = workbook.createCellStyle();
            Font bold = workbook.createFont();
            bold.setBold(true);
            headerStyle.setFont(bold);

            Row header = sheet.createRow(0);
            for (int i = 0; i < headers.length; i++) {
                var cell = header.createCell(i);
                cell.setCellValue(headers[i]);
                cell.setCellStyle(headerStyle);
            }

            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
            int rowIndex = 1;
            for (StockMovement row : rows) {
                Row data = sheet.createRow(rowIndex++);
                Product product = row.getProduct();
                data.createCell(0).setCellValue(row.getId() == null ? "" : String.valueOf(row.getId()));
                data.createCell(1).setCellValue(row.getCreatedAt() == null ? "" : formatter.format(row.getCreatedAt()));
                data.createCell(2).setCellValue(product == null ? "" : nullToEmpty(product.getName()));
                data.createCell(3).setCellValue(product == null ? "" : nullToEmpty(product.getSku()));
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
     * Executes the stringValue operation.
     *
     * @param value Parameter of type {@code Long} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String stringValue(Long value) {
        return value == null ? "" : String.valueOf(value);
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
        String escaped = value.replace("\"", "\"\"");
        return "\"" + escaped + "\"";
    }
}
