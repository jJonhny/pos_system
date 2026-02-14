package com.example.pos_system.controller;

import com.example.pos_system.entity.Supplier;
import com.example.pos_system.entity.SupplierStatus;
import com.example.pos_system.service.SupplierService;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@RequestMapping("/suppliers")
public class SuppliersController {
    private final SupplierService supplierService;

    public SuppliersController(SupplierService supplierService) {
        this.supplierService = supplierService;
    }

    @GetMapping
    public String list(@RequestParam(required = false) String q,
                       @RequestParam(required = false) SupplierStatus status,
                       @RequestParam(required = false) Long editId,
                       Model model) {
        model.addAttribute("suppliers", supplierService.list(q, status));
        model.addAttribute("q", q);
        model.addAttribute("status", status);
        model.addAttribute("statuses", SupplierStatus.values());
        Supplier edit = editId == null ? null : supplierService.get(editId);
        model.addAttribute("editSupplier", edit);
        return "suppliers/list";
    }

    @PostMapping
    public String save(@RequestParam(required = false) Long id,
                       @RequestParam String name,
                       @RequestParam(required = false) String phone,
                       @RequestParam(required = false) String email,
                       @RequestParam(required = false) String address,
                       @RequestParam(required = false) SupplierStatus status,
                       RedirectAttributes redirectAttributes) {
        try {
            Supplier saved = supplierService.save(id, name, phone, email, address, status);
            redirectAttributes.addFlashAttribute("success", "Saved supplier " + saved.getName() + ".");
        } catch (RuntimeException ex) {
            redirectAttributes.addFlashAttribute("error", ex.getMessage());
        }
        return "redirect:/suppliers";
    }

    @PostMapping("/{id}/delete")
    public String delete(@PathVariable Long id, RedirectAttributes redirectAttributes) {
        try {
            supplierService.delete(id);
            redirectAttributes.addFlashAttribute("success", "Supplier deleted.");
        } catch (RuntimeException ex) {
            redirectAttributes.addFlashAttribute("error", ex.getMessage());
        }
        return "redirect:/suppliers";
    }
}
