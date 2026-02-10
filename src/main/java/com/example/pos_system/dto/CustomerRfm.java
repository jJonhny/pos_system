package com.example.pos_system.dto;

public record CustomerRfm(
        String name,
        int recencyDays,
        int frequency,
        double monetary,
        int score
) {}
