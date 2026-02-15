package com.example.pos_system.controller;

import com.example.pos_system.entity.PrinterMode;
import com.example.pos_system.entity.TerminalSettings;
import com.example.pos_system.service.PosHardwareService;
import com.example.pos_system.service.TerminalSettingsService;
import org.springframework.http.MediaType;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.List;

@Controller
@RequestMapping("/pos-setting")
public class PosSettingsController {
    private final TerminalSettingsService terminalSettingsService;
    private final PosHardwareService posHardwareService;

    public PosSettingsController(TerminalSettingsService terminalSettingsService,
                                 PosHardwareService posHardwareService) {
        this.terminalSettingsService = terminalSettingsService;
        this.posHardwareService = posHardwareService;
    }

    @GetMapping
    public String index(@RequestParam(required = false) Long editId, Model model) {
        List<TerminalSettings> terminals = terminalSettingsService.list();
        TerminalSettings edit = null;
        if (editId != null) {
            edit = terminalSettingsService.findById(editId).orElse(null);
        }
        model.addAttribute("terminals", terminals);
        model.addAttribute("editTerminal", edit);
        model.addAttribute("printerModes", PrinterMode.values());
        model.addAttribute("defaultBridgeUrl", TerminalSettingsService.DEFAULT_BRIDGE_URL);
        return "pos-setting/index";
    }

    @PostMapping("/terminals")
    public String save(@RequestParam(required = false) Long id,
                       @RequestParam String terminalId,
                       @RequestParam String name,
                       @RequestParam(required = false) String defaultCurrency,
                       @RequestParam(required = false) String receiptHeader,
                       @RequestParam(required = false) String receiptFooter,
                       @RequestParam(required = false) String taxId,
                       @RequestParam(required = false) PrinterMode printerMode,
                       @RequestParam(required = false) String bridgeUrl,
                       @RequestParam(required = false) Boolean autoPrintEnabled,
                       @RequestParam(required = false) Boolean cameraScannerEnabled,
                       RedirectAttributes redirectAttributes) {
        try {
            TerminalSettings saved = terminalSettingsService.save(
                    id,
                    terminalId,
                    name,
                    defaultCurrency,
                    receiptHeader,
                    receiptFooter,
                    taxId,
                    printerMode,
                    bridgeUrl,
                    autoPrintEnabled,
                    cameraScannerEnabled
            );
            redirectAttributes.addFlashAttribute("success", "Saved settings for terminal " + saved.getTerminalId() + ".");
            return "redirect:/pos-setting?editId=" + saved.getId();
        } catch (RuntimeException ex) {
            redirectAttributes.addFlashAttribute("error", ex.getMessage());
            return "redirect:/pos-setting";
        }
    }

    @PostMapping("/terminals/{id}/delete")
    public String delete(@PathVariable Long id, RedirectAttributes redirectAttributes) {
        terminalSettingsService.delete(id);
        redirectAttributes.addFlashAttribute("success", "Terminal deleted.");
        return "redirect:/pos-setting";
    }

    @PostMapping(value = "/terminals/{id}/test-print", produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public PosHardwareService.PrintResponse testPrint(@PathVariable Long id) {
        TerminalSettings terminal = terminalSettingsService.findById(id)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Terminal not found."));
        return posHardwareService.buildPrinterTestResponse(terminal.getTerminalId());
    }

    @PostMapping(value = "/terminals/{id}/test-drawer", produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public PosHardwareService.DrawerResponse testDrawer(@PathVariable Long id,
                                                        Authentication authentication) {
        TerminalSettings terminal = terminalSettingsService.findById(id)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Terminal not found."));
        String actor = authentication == null ? null : authentication.getName();
        if (actor == null || actor.isBlank()) {
            throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Sign in required.");
        }
        return posHardwareService.openDrawer(actor, terminal.getTerminalId(), null);
    }
}
