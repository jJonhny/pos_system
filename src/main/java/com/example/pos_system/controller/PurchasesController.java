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

    public PurchasesController(PurchaseService purchaseService) {
        this.purchaseService = purchaseService;
    }

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

    @GetMapping("/po/new")
    public String poNew(Model model) {
        model.addAttribute("po", null);
        model.addAttribute("poStatuses", PurchaseOrderStatus.values());
        model.addAttribute("suppliers", purchaseService.listSuppliers());
        model.addAttribute("products", purchaseService.listProducts());
        return "purchases/po-form";
    }

    @GetMapping("/po/{id}/edit")
    public String poEdit(@PathVariable Long id, Model model) {
        PurchaseOrder po = purchaseService.getPurchaseOrder(id);
        model.addAttribute("po", po);
        model.addAttribute("poStatuses", PurchaseOrderStatus.values());
        model.addAttribute("suppliers", purchaseService.listSuppliers());
        model.addAttribute("products", purchaseService.listProducts());
        return "purchases/po-form";
    }

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

    @GetMapping("/po/{id}/receive")
    public String receiveFromPo(@PathVariable Long id, Model model) {
        PurchaseOrder po = purchaseService.getPurchaseOrder(id);
        model.addAttribute("po", po);
        model.addAttribute("suppliers", purchaseService.listSuppliers());
        model.addAttribute("products", purchaseService.listProducts());
        return "purchases/grn-form";
    }

    @GetMapping("/receive")
    public String receiveStandalone(@RequestParam(required = false) Long poId, Model model) {
        PurchaseOrder po = poId == null ? null : purchaseService.getPurchaseOrder(poId);
        model.addAttribute("po", po);
        model.addAttribute("suppliers", purchaseService.listSuppliers());
        model.addAttribute("products", purchaseService.listProducts());
        return "purchases/grn-form";
    }

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

    @GetMapping("/po/line")
    public String poLine(Model model) {
        model.addAttribute("products", purchaseService.listProducts());
        return "purchases/fragments :: poLine";
    }

    @GetMapping("/grn/line")
    public String grnLine(Model model) {
        model.addAttribute("products", purchaseService.listProducts());
        return "purchases/fragments :: grnLine";
    }

    @GetMapping("/line/remove")
    public String removeLine() {
        return "purchases/fragments :: empty";
    }

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

    private <T> T at(List<T> list, int index) {
        if (list == null || index < 0 || index >= list.size()) {
            return null;
        }
        return list.get(index);
    }
}
