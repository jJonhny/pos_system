package com.example.pos_system.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class ManagementPagesController {

    @GetMapping("/commodity")
    public String commodity() {
        return "commodity/index";
    }
}
