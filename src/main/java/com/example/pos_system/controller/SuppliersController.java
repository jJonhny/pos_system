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

    /**
     * Executes the SuppliersController operation.
     * <p>Return value: A fully initialized SuppliersController instance.</p>
     *
     * @param supplierService Parameter of type {@code SupplierService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public SuppliersController(SupplierService supplierService) {
        this.supplierService = supplierService;
    }

    /**
     * Executes the list operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code SupplierStatus} used by this operation.
     * @param editId Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the list operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code SupplierStatus} used by this operation.
     * @param editId Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the list operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code SupplierStatus} used by this operation.
     * @param editId Parameter of type {@code Long} used by this operation.
     * @param model Parameter of type {@code Model} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the save operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param phone Parameter of type {@code String} used by this operation.
     * @param email Parameter of type {@code String} used by this operation.
     * @param address Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code SupplierStatus} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the save operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param phone Parameter of type {@code String} used by this operation.
     * @param email Parameter of type {@code String} used by this operation.
     * @param address Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code SupplierStatus} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the save operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param phone Parameter of type {@code String} used by this operation.
     * @param email Parameter of type {@code String} used by this operation.
     * @param address Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code SupplierStatus} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the delete operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the delete operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the delete operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param redirectAttributes Parameter of type {@code RedirectAttributes} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
