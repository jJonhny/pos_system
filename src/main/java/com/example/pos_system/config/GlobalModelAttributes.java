package com.example.pos_system.config;

import com.example.pos_system.repository.ProductRepo;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ModelAttribute;

@ControllerAdvice
public class GlobalModelAttributes {
    private final ProductRepo productRepo;

    public GlobalModelAttributes(ProductRepo productRepo) {
        this.productRepo = productRepo;
    }

    @ModelAttribute("lowStockCount")
    public long lowStockCount() {
        return productRepo.countLowStock();
    }
}
