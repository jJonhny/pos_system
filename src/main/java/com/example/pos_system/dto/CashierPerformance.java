package com.example.pos_system.dto;

public record CashierPerformance(
        String cashier,
        double revenue,
        int transactions,
        int items,
        double avgOrderValue,
        double itemsPerMinute
) {}
