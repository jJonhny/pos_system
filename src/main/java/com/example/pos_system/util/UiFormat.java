package com.example.pos_system.util;

import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import com.example.pos_system.entity.UnitType;

@Component("uiFormat")
public class UiFormat {
    private static final DateTimeFormatter DATE_TIME_FMT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
    private static final DateTimeFormatter DATE_FMT = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    public String money(BigDecimal value) {
        if (value == null) return "-";
        BigDecimal scaled = value.setScale(2, RoundingMode.HALF_UP);
        return "$" + scaled.toPlainString();
    }

    public String dateTime(LocalDateTime value) {
        if (value == null) return "-";
        return value.format(DATE_TIME_FMT);
    }

    public String date(LocalDate value) {
        if (value == null) return "-";
        return value.format(DATE_FMT);
    }

    public String unitLabel(UnitType unitType) {
        if (unitType == null) return "pc";
        return switch (unitType) {
            case BOX -> "box";
            case CASE -> "case";
            default -> "pc";
        };
    }
}
