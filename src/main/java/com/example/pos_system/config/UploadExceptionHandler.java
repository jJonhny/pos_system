package com.example.pos_system.config;

import jakarta.servlet.http.HttpServletRequest;
import org.apache.tomcat.util.http.InvalidParameterException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.multipart.MaxUploadSizeExceededException;
import org.springframework.web.multipart.MultipartException;

@ControllerAdvice
public class UploadExceptionHandler {

    @ExceptionHandler({
            MaxUploadSizeExceededException.class,
            MultipartException.class,
            InvalidParameterException.class
    })
    public String handleUploadExceptions(Exception ex, HttpServletRequest request) throws Exception {
        if (!isUploadTooLarge(ex)) {
            throw ex;
        }
        String uri = request.getRequestURI() == null ? "" : request.getRequestURI();
        if (uri.startsWith("/products")) {
            return "redirect:/products?error=uploadTooLarge";
        }
        if (uri.startsWith("/categories")) {
            return "redirect:/categories?error=uploadTooLarge";
        }
        return "redirect:/?error=uploadTooLarge";
    }

    private boolean isUploadTooLarge(Throwable ex) {
        Throwable cursor = ex;
        while (cursor != null) {
            String className = cursor.getClass().getName();
            String message = cursor.getMessage();
            if (cursor instanceof MaxUploadSizeExceededException) {
                return true;
            }
            if ("org.apache.tomcat.util.http.fileupload.impl.FileSizeLimitExceededException".equals(className)) {
                return true;
            }
            if (message != null && message.toLowerCase().contains("maximum permitted size")) {
                return true;
            }
            cursor = cursor.getCause();
        }
        return false;
    }
}
