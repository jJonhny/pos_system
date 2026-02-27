package com.example.pos_system.api.v1.dto.common;

import java.time.Instant;
import java.util.List;

public record ApiEnvelope<T>(
        boolean success,
        String code,
        String message,
        T data,
        List<String> details,
        Instant timestamp,
        String traceId
) {
    public static <T> ApiEnvelope<T> success(String code, String message, T data, String traceId) {
        return new ApiEnvelope<>(true, code, message, data, List.of(), Instant.now(), traceId);
    }

    public static ApiEnvelope<Void> error(String code, String message, List<String> details, String traceId) {
        List<String> safeDetails = details == null ? List.of() : details;
        return new ApiEnvelope<>(false, code, message, null, safeDetails, Instant.now(), traceId);
    }
}
