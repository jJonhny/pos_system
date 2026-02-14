package com.example.pos_system.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class ManagementPagesController {

    @GetMapping("/marketing")
    public String marketing() {
        return "marketing/index";
    }

    @GetMapping("/pos-setting")
    public String posSetting() {
        return "pos-setting/index";
    }
}
