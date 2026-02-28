package com.devcore.pos_system.api.v1.support;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.util.UUID;

public final class ApiTrace {
    public static final String TRACE_ID_HEADER = "X-Trace-Id";
    public static final String TRACE_ID_ATTRIBUTE = "traceId";

    private ApiTrace() {
    }

    public static String resolve(HttpServletRequest request) {
        if (request == null) {
            return UUID.randomUUID().toString();
        }

        String header = request.getHeader(TRACE_ID_HEADER);
        if (header != null && !header.isBlank()) {
            return header.trim();
        }

        Object attr = request.getAttribute(TRACE_ID_ATTRIBUTE);
        if (attr != null) {
            return String.valueOf(attr);
        }

        return UUID.randomUUID().toString();
    }

    public static String ensure(HttpServletRequest request, HttpServletResponse response) {
        String traceId = resolve(request);
        if (request != null) {
            request.setAttribute(TRACE_ID_ATTRIBUTE, traceId);
        }
        if (response != null) {
            response.setHeader(TRACE_ID_HEADER, traceId);
        }
        return traceId;
    }
}
