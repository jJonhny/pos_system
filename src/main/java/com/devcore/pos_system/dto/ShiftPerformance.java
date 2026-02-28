package com.devcore.pos_system.dto;

public record ShiftPerformance(
        String cashier,
        double hours,
        double totalSales,
        double salesPerHour,
        double cashVariance
) {}
