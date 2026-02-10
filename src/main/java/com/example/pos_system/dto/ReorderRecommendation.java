package com.example.pos_system.dto;

public record ReorderRecommendation(
        String name,
        Integer stockQty,
        double avgDaily,
        double daysOfStock,
        boolean lowStock
) {}
