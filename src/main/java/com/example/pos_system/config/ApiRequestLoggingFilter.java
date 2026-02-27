package com.example.pos_system.config;

import com.example.pos_system.api.v1.support.ApiTrace;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
@Order(Ordered.HIGHEST_PRECEDENCE + 20)
public class ApiRequestLoggingFilter extends OncePerRequestFilter {
    private static final Logger log = LoggerFactory.getLogger(ApiRequestLoggingFilter.class);

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        String uri = request.getRequestURI();
        return uri == null || !uri.startsWith("/api/");
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain) throws ServletException, IOException {
        long startedAt = System.nanoTime();
        String traceId = ApiTrace.ensure(request, response);
        try {
            filterChain.doFilter(request, response);
        } finally {
            long durationMs = Math.round((System.nanoTime() - startedAt) / 1_000_000.0d);
            log.info(
                    "api_request method={} path={} status={} durationMs={} traceId={} user={}",
                    request.getMethod(),
                    request.getRequestURI(),
                    response.getStatus(),
                    durationMs,
                    traceId,
                    currentPrincipal()
            );
        }
    }

    private String currentPrincipal() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !authentication.isAuthenticated()) {
            return "anonymous";
        }
        String name = authentication.getName();
        if (name == null || name.isBlank()) {
            return "anonymous";
        }
        return name;
    }
}
