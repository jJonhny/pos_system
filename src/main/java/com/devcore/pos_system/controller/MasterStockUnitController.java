package com.devcore.pos_system.controller;

import com.devcore.pos_system.entity.UnitOfMeasure;
import com.devcore.pos_system.service.MasterStockUnitService;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.List;
import java.util.Map;

@Controller
@RequestMapping("/msw")
public class MasterStockUnitController {
    private final MasterStockUnitService masterStockUnitService;

    public MasterStockUnitController(MasterStockUnitService masterStockUnitService) {
        this.masterStockUnitService = masterStockUnitService;
    }

    @GetMapping
    public String list(Model model) {
        List<UnitOfMeasure> units = masterStockUnitService.listForManagement();
        Map<Long, Long> usageCounts = masterStockUnitService.usageCounts(units.stream().map(UnitOfMeasure::getId).toList());
        model.addAttribute("units", units);
        model.addAttribute("usageCounts", usageCounts);
        return "msw/list";
    }

    @GetMapping("/{id}/edit")
    public String editForm(@PathVariable Long id, Model model) {
        UnitOfMeasure unit = masterStockUnitService.require(id);
        model.addAttribute("unit", unit);
        Long usageCount = masterStockUnitService.usageCounts(List.of(unit.getId())).getOrDefault(unit.getId(), 0L);
        model.addAttribute("usageCount", usageCount);
        return "msw/form";
    }

    @PostMapping
    public String create(@RequestParam String code,
                         @RequestParam String name,
                         @RequestParam(required = false) Integer precisionScale,
                         @RequestParam(required = false) Boolean active,
                         RedirectAttributes redirectAttributes) {
        try {
            masterStockUnitService.create(code, name, precisionScale, active);
            redirectAttributes.addFlashAttribute("success", "MSW added.");
        } catch (IllegalArgumentException ex) {
            redirectAttributes.addFlashAttribute("error", ex.getMessage());
        }
        return "redirect:/msw";
    }

    @PostMapping("/{id}")
    public String update(@PathVariable Long id,
                         @RequestParam String code,
                         @RequestParam String name,
                         @RequestParam(required = false) Integer precisionScale,
                         @RequestParam(required = false) Boolean active,
                         RedirectAttributes redirectAttributes) {
        try {
            masterStockUnitService.update(id, code, name, precisionScale, active);
            redirectAttributes.addFlashAttribute("success", "MSW updated.");
            return "redirect:/msw";
        } catch (IllegalArgumentException ex) {
            redirectAttributes.addFlashAttribute("error", ex.getMessage());
            return "redirect:/msw/" + id + "/edit";
        }
    }

    @PostMapping("/{id}/delete")
    public String delete(@PathVariable Long id, RedirectAttributes redirectAttributes) {
        try {
            masterStockUnitService.delete(id);
            redirectAttributes.addFlashAttribute("success", "MSW deleted.");
        } catch (IllegalStateException | IllegalArgumentException ex) {
            redirectAttributes.addFlashAttribute("error", ex.getMessage());
        }
        return "redirect:/msw";
    }
}
