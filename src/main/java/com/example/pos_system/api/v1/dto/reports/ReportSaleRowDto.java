package com.example.pos_system.api.v1.dto.reports;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public record ReportSaleRowDto(
        Long id,
        LocalDateTime createdAt,
        String cashierUsername,
        String terminalId,
        String paymentMethod,
        String status,
        BigDecimal subtotal,
        BigDecimal discount,
        BigDecimal tax,
        BigDecimal total,
        BigDecimal refundedTotal,
        int itemCount
) {
}
