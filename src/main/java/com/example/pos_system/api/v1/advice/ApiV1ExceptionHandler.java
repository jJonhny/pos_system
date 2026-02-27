package com.example.pos_system.api.v1.advice;

import com.example.pos_system.api.v1.dto.common.ApiEnvelope;
import com.example.pos_system.api.v1.exception.ApiBadRequestException;
import com.example.pos_system.api.v1.exception.ApiNotFoundException;
import com.example.pos_system.api.v1.support.ApiTrace;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.ConstraintViolationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.List;

@RestControllerAdvice(basePackages = "com.example.pos_system.api")
public class ApiV1ExceptionHandler {
    private static final Logger log = LoggerFactory.getLogger(ApiV1ExceptionHandler.class);

    @ExceptionHandler(ApiNotFoundException.class)
    public ResponseEntity<ApiEnvelope<Void>> handleNotFound(ApiNotFoundException ex, HttpServletRequest request) {
        return build(HttpStatus.NOT_FOUND, "NOT_FOUND", ex.getMessage(), List.of(), request);
    }

    @ExceptionHandler(ApiBadRequestException.class)
    public ResponseEntity<ApiEnvelope<Void>> handleBadRequest(ApiBadRequestException ex, HttpServletRequest request) {
        return build(HttpStatus.BAD_REQUEST, "BAD_REQUEST", ex.getMessage(), List.of(), request);
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ApiEnvelope<Void>> handleValidation(MethodArgumentNotValidException ex, HttpServletRequest request) {
        List<String> details = ex.getBindingResult()
                .getFieldErrors()
                .stream()
                .map(this::toValidationMessage)
                .toList();
        return build(HttpStatus.BAD_REQUEST, "VALIDATION_FAILED", "Request validation failed.", details, request);
    }

    @ExceptionHandler(ConstraintViolationException.class)
    public ResponseEntity<ApiEnvelope<Void>> handleConstraint(ConstraintViolationException ex, HttpServletRequest request) {
        List<String> details = ex.getConstraintViolations()
                .stream()
                .map(v -> v.getPropertyPath() + " " + v.getMessage())
                .toList();
        return build(HttpStatus.BAD_REQUEST, "VALIDATION_FAILED", "Request validation failed.", details, request);
    }

    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ResponseEntity<ApiEnvelope<Void>> handleUnreadable(HttpMessageNotReadableException ex, HttpServletRequest request) {
        return build(HttpStatus.BAD_REQUEST, "MALFORMED_JSON", "Malformed request body.", List.of(), request);
    }

    @ExceptionHandler(DataIntegrityViolationException.class)
    public ResponseEntity<ApiEnvelope<Void>> handleConstraintViolation(DataIntegrityViolationException ex, HttpServletRequest request) {
        return build(HttpStatus.CONFLICT, "DATA_CONFLICT", "Data integrity constraint violated.", List.of(), request);
    }

    @ExceptionHandler(AccessDeniedException.class)
    public ResponseEntity<ApiEnvelope<Void>> handleAccessDenied(AccessDeniedException ex, HttpServletRequest request) {
        return build(HttpStatus.FORBIDDEN, "ACCESS_DENIED", "Access denied.", List.of(), request);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ApiEnvelope<Void>> handleUnexpected(Exception ex, HttpServletRequest request) {
        log.error("Unhandled API exception", ex);
        return build(HttpStatus.INTERNAL_SERVER_ERROR, "INTERNAL_ERROR", "An unexpected error occurred.", List.of(), request);
    }

    private ResponseEntity<ApiEnvelope<Void>> build(HttpStatus status,
                                                    String code,
                                                    String message,
                                                    List<String> details,
                                                    HttpServletRequest request) {
        return ResponseEntity.status(status).body(ApiEnvelope.error(
                code,
                message,
                details,
                ApiTrace.resolve(request)
        ));
    }

    private String toValidationMessage(FieldError fieldError) {
        String field = fieldError.getField();
        String message = fieldError.getDefaultMessage();
        if (message == null || message.isBlank()) {
            message = "invalid value";
        }
        return field + ": " + message;
    }
}
