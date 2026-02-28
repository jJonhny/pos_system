package com.devcore.pos_system.dto;

import java.time.Instant;
import java.util.List;

public record ApiErrorResponse(
        String code,
        String message,
        List<String> details,
        Instant timestamp,
        String traceId
) {
}
