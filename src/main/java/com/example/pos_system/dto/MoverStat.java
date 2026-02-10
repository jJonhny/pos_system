package com.example.pos_system.dto;

public record MoverStat(
        String name,
        int qtySold,
        Integer stockQty,
        Double daysOfStock
) {}
