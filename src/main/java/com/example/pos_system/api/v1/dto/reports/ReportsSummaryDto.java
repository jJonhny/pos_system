package com.example.pos_system.api.v1.dto.reports;

import java.math.BigDecimal;

public record ReportsSummaryDto(
        long salesCount,
        BigDecimal totalRevenue,
        BigDecimal averageTicket,
        long refundCount,
        BigDecimal refundTotal,
        long shiftCount
) {
}
