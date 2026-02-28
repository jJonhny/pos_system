package com.devcore.pos_system.api.v1.dto.reports;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public record ReportShiftRowDto(
        Long id,
        String cashierUsername,
        String terminalId,
        String status,
        LocalDateTime openedAt,
        LocalDateTime closedAt,
        BigDecimal totalSales,
        BigDecimal cashTotal,
        BigDecimal cardTotal,
        BigDecimal qrTotal,
        BigDecimal expectedCash,
        BigDecimal closingCash,
        BigDecimal varianceCash
) {
}
