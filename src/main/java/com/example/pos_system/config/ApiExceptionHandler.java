package com.example.pos_system.config;

import com.example.pos_system.dto.ApiErrorResponse;
import com.example.pos_system.service.I18nService;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.server.ResponseStatusException;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

@RestControllerAdvice
public class ApiExceptionHandler {
    private final I18nService i18nService;

    /**
     * Executes the ApiExceptionHandler operation.
     * <p>Return value: A fully initialized ApiExceptionHandler instance.</p>
     *
     * @param i18nService Parameter of type {@code I18nService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ApiExceptionHandler(I18nService i18nService) {
        this.i18nService = i18nService;
    }

    /**
     * Executes the handleResponseStatus operation.
     *
     * @param ex Parameter of type {@code ResponseStatusException} used by this operation.
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code ResponseEntity<ApiErrorResponse>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the handleResponseStatus operation.
     *
     * @param ex Parameter of type {@code ResponseStatusException} used by this operation.
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code ResponseEntity<ApiErrorResponse>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the handleResponseStatus operation.
     *
     * @param ex Parameter of type {@code ResponseStatusException} used by this operation.
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code ResponseEntity<ApiErrorResponse>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @ExceptionHandler(ResponseStatusException.class)
    public ResponseEntity<ApiErrorResponse> handleResponseStatus(ResponseStatusException ex, HttpServletRequest request) {
        if (!expectsJson(request)) {
            throw ex;
        }
        int status = ex.getStatusCode().value();
        String code = "error.http." + status;
        String message = ex.getReason();
        if (message == null || message.isBlank()) {
            message = i18nService.msg(code);
        }
        return ResponseEntity.status(ex.getStatusCode()).body(new ApiErrorResponse(
                code,
                message,
                List.of(),
                Instant.now(),
                traceId(request)
        ));
    }

    /**
     * Executes the handleIllegalArgument operation.
     *
     * @param ex Parameter of type {@code IllegalArgumentException} used by this operation.
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code ResponseEntity<ApiErrorResponse>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the handleIllegalArgument operation.
     *
     * @param ex Parameter of type {@code IllegalArgumentException} used by this operation.
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code ResponseEntity<ApiErrorResponse>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the handleIllegalArgument operation.
     *
     * @param ex Parameter of type {@code IllegalArgumentException} used by this operation.
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code ResponseEntity<ApiErrorResponse>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<ApiErrorResponse> handleIllegalArgument(IllegalArgumentException ex, HttpServletRequest request) {
        if (!expectsJson(request)) {
            throw ex;
        }
        String message = ex.getMessage();
        if (message == null || message.isBlank()) {
            message = i18nService.msg("error.badRequest");
        }
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(new ApiErrorResponse(
                "error.badRequest",
                message,
                List.of(),
                Instant.now(),
                traceId(request)
        ));
    }

    /**
     * Executes the expectsJson operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean expectsJson(HttpServletRequest request) {
        if (request == null) return false;
        String uri = request.getRequestURI();
        if (uri != null && (uri.startsWith("/api/v1/")
                || uri.startsWith("/pos/products/feed")
                || uri.startsWith("/pos/checkout/")
                || uri.startsWith("/pos/drawer/"))) {
            return true;
        }
        String accept = request.getHeader("Accept");
        if (accept != null && accept.contains("application/json")) {
            return true;
        }
        String requestedWith = request.getHeader("X-Requested-With");
        return requestedWith != null && requestedWith.contains("XMLHttpRequest");
    }

    /**
     * Executes the traceId operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String traceId(HttpServletRequest request) {
        if (request == null) return UUID.randomUUID().toString();
        String header = request.getHeader("X-Trace-Id");
        if (header != null && !header.isBlank()) return header;
        Object attr = request.getAttribute("traceId");
        if (attr != null) return String.valueOf(attr);
        return UUID.randomUUID().toString();
    }
}
