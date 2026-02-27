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
    /**
     * Executes the handleUploadExceptions operation.
     *
     * @param ex Parameter of type {@code Exception} used by this operation.
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @return {@code String} Result produced by this operation.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

    /**
     * Executes the isUploadTooLarge operation.
     *
     * @param ex Parameter of type {@code Throwable} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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
