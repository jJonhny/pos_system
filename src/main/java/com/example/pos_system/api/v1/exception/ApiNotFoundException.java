package com.example.pos_system.api.v1.exception;

public class ApiNotFoundException extends RuntimeException {
    public ApiNotFoundException(String message) {
        super(message);
    }
}
