package com.example.pos_system.controller;

import com.example.pos_system.entity.GoodsReceipt;
import com.example.pos_system.entity.PurchaseOrder;
import com.example.pos_system.entity.PurchaseOrderStatus;
import com.example.pos_system.service.PurchaseService;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@Controller
@RequestMapping("/purchases")
public class PurchasesController {
    private final PurchaseService purchaseService;

    /**
     * Executes the PurchasesController operation.
     * <p>Return value: A fully initialized PurchasesController instance.</p>
     *
     * @param purchaseService Parameter of type {@code PurchaseService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public PurchasesController(PurchaseService purchaseService) {
        this.purchaseService = purchaseService;
    }

    /**
     * Executes the poList operation.
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
     * Executes the poList operation.
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
     * Executes the poList operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/po")
    public String poList(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                         @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                         @RequestParam(required = false) Long supplierId,
                         Model model) {
        model.addAttribute("purchaseOrders", purchaseService.listPurchaseOrders());
        model.addAttribute("suppliers", purchaseService.listSuppliers());
        model.addAttribute("goodsReceipts", purchaseService.listGoodsReceipts(from, to, supplierId));
        model.addAttribute("from", from);
        model.addAttribute("to", to);
        model.addAttribute("supplierId", supplierId);
        return "purchases/po-list";
    }

    /**
     * Executes the poNew operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the poNew operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the poNew operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/po/new")
    public String poNew(Model model) {
        model.addAttribute("po", null);
        model.addAttribute("poStatuses", PurchaseOrderStatus.values());
        model.addAttribute("suppliers", purchaseService.listSuppliers());
        model.addAttribute("products", purchaseService.listProducts());
        return "purchases/po-form";
    }

    /**
     * Executes the poEdit operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the poEdit operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the poEdit operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/po/{id}/edit")
    public String poEdit(@PathVariable Long id, Model model) {
        PurchaseOrder po = purchaseService.getPurchaseOrder(id);
        model.addAttribute("po", po);
        model.addAttribute("poStatuses", PurchaseOrderStatus.values());
        model.addAttribute("suppliers", purchaseService.listSuppliers());
        model.addAttribute("products", purchaseService.listProducts());
        return "purchases/po-form";
    }

    /**
     * Executes the savePo operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @param status Parameter of type {@code PurchaseOrderStatus} used by this operation.
     * @param currency Parameter of type {@code String} used by this operation.
     * @param expectedAt Parameter of type {@code LocalDate} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @param lineProductId Parameter of type {@code List<Long>} used by this operation.
     * @param lineOrderedQty Parameter of type {@code List<Integer>} used by this operation.
     * @param lineUnitCost Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param lineTax Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param lineDiscount Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the savePo operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @param status Parameter of type {@code PurchaseOrderStatus} used by this operation.
     * @param currency Parameter of type {@code String} used by this operation.
     * @param expectedAt Parameter of type {@code LocalDate} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @param lineProductId Parameter of type {@code List<Long>} used by this operation.
     * @param lineOrderedQty Parameter of type {@code List<Integer>} used by this operation.
     * @param lineUnitCost Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param lineTax Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param lineDiscount Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the savePo operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param supplierId Parameter of type {@code Long} used by this operation.
     * @param status Parameter of type {@code PurchaseOrderStatus} used by this operation.
     * @param currency Parameter of type {@code String} used by this operation.
     * @param expectedAt Parameter of type {@code LocalDate} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @param lineProductId Parameter of type {@code List<Long>} used by this operation.
     * @param lineOrderedQty Parameter of type {@code List<Integer>} used by this operation.
     * @param lineUnitCost Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param lineTax Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param lineDiscount Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/po")
    public String savePo(@RequestParam(required = false) Long id,
                         @RequestParam Long supplierId,
                         @RequestParam(required = false) PurchaseOrderStatus status,
                         @RequestParam(required = false) String currency,
                         @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate expectedAt,
                         @RequestParam(required = false) String notes,
                         @RequestParam(required = false) List<Long> lineProductId,
                         @RequestParam(required = false) List<Integer> lineOrderedQty,
                         @RequestParam(required = false) List<BigDecimal> lineUnitCost,
                         @RequestParam(required = false) List<BigDecimal> lineTax,
                         @RequestParam(required = false) List<BigDecimal> lineDiscount,
                         RedirectAttributes redirectAttributes) {
        try {
            PurchaseOrder saved = purchaseService.savePurchaseOrder(
                    id,
                    supplierId,
                    status,
                    currency,
                    expectedAt,
                    notes,
                    buildPoLines(lineProductId, lineOrderedQty, lineUnitCost, lineTax, lineDiscount)
            );
            redirectAttributes.addFlashAttribute("success", "Purchase order #" + saved.getId() + " saved.");
            return "redirect:/purchases/po";
        } catch (RuntimeException ex) {
            redirectAttributes.addFlashAttribute("error", ex.getMessage());
            return "redirect:" + (id == null ? "/purchases/po/new" : ("/purchases/po/" + id + "/edit"));
        }
    }

    /**
     * Executes the receiveFromPo operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the receiveFromPo operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the receiveFromPo operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/po/{id}/receive")
    public String receiveFromPo(@PathVariable Long id, Model model) {
        PurchaseOrder po = purchaseService.getPurchaseOrder(id);
        model.addAttribute("po", po);
        model.addAttribute("suppliers", purchaseService.listSuppliers());
        model.addAttribute("products", purchaseService.listProducts());
        return "purchases/grn-form";
    }

    /**
     * Executes the receiveStandalone operation.
     *
     * @param poId Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the receiveStandalone operation.
     *
     * @param poId Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the receiveStandalone operation.
     *
     * @param poId Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/receive")
    public String receiveStandalone(@RequestParam(required = false) Long poId, Model model) {
        PurchaseOrder po = poId == null ? null : purchaseService.getPurchaseOrder(poId);
        model.addAttribute("po", po);
        model.addAttribute("suppliers", purchaseService.listSuppliers());
        model.addAttribute("products", purchaseService.listProducts());
        return "purchases/grn-form";
    }

    /**
     * Executes the postReceive operation.
     *
     * @param poId Parameter of type {@code Long} used by this operation.
     * @param invoiceNo Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param grnProductId Parameter of type {@code List<Long>} used by this operation.
     * @param grnReceivedQty Parameter of type {@code List<Integer>} used by this operation.
     * @param grnUnitCost Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the postReceive operation.
     *
     * @param poId Parameter of type {@code Long} used by this operation.
     * @param invoiceNo Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param grnProductId Parameter of type {@code List<Long>} used by this operation.
     * @param grnReceivedQty Parameter of type {@code List<Integer>} used by this operation.
     * @param grnUnitCost Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the postReceive operation.
     *
     * @param poId Parameter of type {@code Long} used by this operation.
     * @param invoiceNo Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param grnProductId Parameter of type {@code List<Long>} used by this operation.
     * @param grnReceivedQty Parameter of type {@code List<Integer>} used by this operation.
     * @param grnUnitCost Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/receive")
    public String postReceive(@RequestParam(required = false) Long poId,
                              @RequestParam(required = false) String invoiceNo,
                              @RequestParam(required = false) String notes,
                              @RequestParam(required = false) String terminalId,
                              @RequestParam(required = false) List<Long> grnProductId,
                              @RequestParam(required = false) List<Integer> grnReceivedQty,
                              @RequestParam(required = false) List<BigDecimal> grnUnitCost,
                              RedirectAttributes redirectAttributes) {
        try {
            GoodsReceipt grn = purchaseService.postGoodsReceipt(
                    poId,
                    invoiceNo,
                    notes,
                    terminalId,
                    buildGrnLines(grnProductId, grnReceivedQty, grnUnitCost)
            );
            redirectAttributes.addFlashAttribute("success", "Goods receipt #" + grn.getId() + " posted.");
            return "redirect:/purchases/po";
        } catch (RuntimeException ex) {
            redirectAttributes.addFlashAttribute("error", ex.getMessage());
            if (poId == null) {
                return "redirect:/purchases/receive";
            }
            return "redirect:/purchases/po/" + poId + "/receive";
        }
    }

    /**
     * Executes the postReceiveForPo operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param invoiceNo Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param grnProductId Parameter of type {@code List<Long>} used by this operation.
     * @param grnReceivedQty Parameter of type {@code List<Integer>} used by this operation.
     * @param grnUnitCost Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the postReceiveForPo operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param invoiceNo Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param grnProductId Parameter of type {@code List<Long>} used by this operation.
     * @param grnReceivedQty Parameter of type {@code List<Integer>} used by this operation.
     * @param grnUnitCost Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the postReceiveForPo operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param invoiceNo Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param grnProductId Parameter of type {@code List<Long>} used by this operation.
     * @param grnReceivedQty Parameter of type {@code List<Integer>} used by this operation.
     * @param grnUnitCost Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PostMapping("/po/{id}/receive")
    public String postReceiveForPo(@PathVariable Long id,
                                   @RequestParam(required = false) String invoiceNo,
                                   @RequestParam(required = false) String notes,
                                   @RequestParam(required = false) String terminalId,
                                   @RequestParam(required = false) List<Long> grnProductId,
                                   @RequestParam(required = false) List<Integer> grnReceivedQty,
                                   @RequestParam(required = false) List<BigDecimal> grnUnitCost,
                                   RedirectAttributes redirectAttributes) {
        return postReceive(id, invoiceNo, notes, terminalId, grnProductId, grnReceivedQty, grnUnitCost, redirectAttributes);
    }

    /**
     * Executes the poLine operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the poLine operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the poLine operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/po/line")
    public String poLine(Model model) {
        model.addAttribute("products", purchaseService.listProducts());
        return "purchases/fragments :: poLine";
    }

    /**
     * Executes the grnLine operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the grnLine operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the grnLine operation.
     *
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/grn/line")
    public String grnLine(Model model) {
        model.addAttribute("products", purchaseService.listProducts());
        return "purchases/fragments :: grnLine";
    }

    /**
     * Executes the removeLine operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the removeLine operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the removeLine operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @GetMapping("/line/remove")
    public String removeLine() {
        return "purchases/fragments :: empty";
    }

    /**
     * Executes the buildPoLines operation.
     *
     * @param productIds Parameter of type {@code List<Long>} used by this operation.
     * @param ordered Parameter of type {@code List<Integer>} used by this operation.
     * @param unitCosts Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param taxes Parameter of type {@code List<BigDecimal>} used by this operation.
     * @param discounts Parameter of type {@code List<BigDecimal>} used by this operation.
     * @return {@code List<PurchaseService.PurchaseOrderLineInput>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<PurchaseService.PurchaseOrderLineInput> buildPoLines(List<Long> productIds,
                                                                      List<Integer> ordered,
                                                                      List<BigDecimal> unitCosts,
                                                                      List<BigDecimal> taxes,
                                                                      List<BigDecimal> discounts) {
        int size = maxSize(productIds, ordered, unitCosts, taxes, discounts);
        List<PurchaseService.PurchaseOrderLineInput> lines = new ArrayList<>();
        for (int i = 0; i < size; i++) {
            lines.add(new PurchaseService.PurchaseOrderLineInput(
                    at(productIds, i),
                    at(ordered, i),
                    at(unitCosts, i),
                    at(taxes, i),
                    at(discounts, i)
            ));
        }
        return lines;
    }

    /**
     * Executes the buildGrnLines operation.
     *
     * @param productIds Parameter of type {@code List<Long>} used by this operation.
     * @param qty Parameter of type {@code List<Integer>} used by this operation.
     * @param unitCost Parameter of type {@code List<BigDecimal>} used by this operation.
     * @return {@code List<PurchaseService.GoodsReceiptLineInput>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<PurchaseService.GoodsReceiptLineInput> buildGrnLines(List<Long> productIds,
                                                                       List<Integer> qty,
                                                                       List<BigDecimal> unitCost) {
        int size = maxSize(productIds, qty, unitCost);
        List<PurchaseService.GoodsReceiptLineInput> lines = new ArrayList<>();
        for (int i = 0; i < size; i++) {
            lines.add(new PurchaseService.GoodsReceiptLineInput(
                    at(productIds, i),
                    at(qty, i),
                    at(unitCost, i)
            ));
        }
        return lines;
    }

    /**
     * Executes the maxSize operation.
     *
     * @param lists Parameter of type {@code List<?>...} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the maxSize operation.
     *
     * @param lists Parameter of type {@code List<?>...} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the maxSize operation.
     *
     * @param lists Parameter of type {@code List<?>...} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @SafeVarargs
    private int maxSize(List<?>... lists) {
        int max = 0;
        for (List<?> list : lists) {
            if (list != null && list.size() > max) {
                max = list.size();
            }
        }
        return max;
    }

    /**
     * Executes the at operation.
     *
     * @param list Parameter of type {@code List<T>} used by this operation.
     * @param index Parameter of type {@code int} used by this operation.
     * @return {@code T} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private <T> T at(List<T> list, int index) {
        if (list == null || index < 0 || index >= list.size()) {
            return null;
        }
        return list.get(index);
    }
}
