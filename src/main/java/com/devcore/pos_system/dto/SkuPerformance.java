package com.devcore.pos_system.dto;

public record SkuPerformance(
        String name,
        double revenue,
        int qty,
        double profit,
        double marginPercent
) {}
