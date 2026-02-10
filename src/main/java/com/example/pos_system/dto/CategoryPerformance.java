package com.example.pos_system.dto;

public record CategoryPerformance(
        String name,
        double revenue,
        double profit,
        double marginPercent
) {}
