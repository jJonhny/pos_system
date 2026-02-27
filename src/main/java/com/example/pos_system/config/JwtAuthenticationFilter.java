package com.example.pos_system.config;

import com.example.pos_system.service.AppUserDetailsService;
import com.example.pos_system.service.JwtTokenService;
import io.jsonwebtoken.Claims;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
public class JwtAuthenticationFilter extends OncePerRequestFilter {
    private final JwtTokenService jwtTokenService;
    private final AppUserDetailsService appUserDetailsService;

    /**
     * Executes the JwtAuthenticationFilter operation.
     * <p>Return value: A fully initialized JwtAuthenticationFilter instance.</p>
     *
     * @param jwtTokenService Parameter of type {@code JwtTokenService} used by this operation.
     * @param appUserDetailsService Parameter of type {@code AppUserDetailsService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public JwtAuthenticationFilter(JwtTokenService jwtTokenService, AppUserDetailsService appUserDetailsService) {
        this.jwtTokenService = jwtTokenService;
        this.appUserDetailsService = appUserDetailsService;
    }

    /**
     * Executes the doFilterInternal operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param filterChain Parameter of type {@code FilterChain} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws ServletException If the operation cannot complete successfully.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the doFilterInternal operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param filterChain Parameter of type {@code FilterChain} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws ServletException If the operation cannot complete successfully.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the doFilterInternal operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param filterChain Parameter of type {@code FilterChain} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws ServletException If the operation cannot complete successfully.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain) throws ServletException, IOException {
        String authorization = request.getHeader("Authorization");
        if (authorization == null || !authorization.startsWith("Bearer ")) {
            filterChain.doFilter(request, response);
            return;
        }

        if (SecurityContextHolder.getContext().getAuthentication() != null) {
            filterChain.doFilter(request, response);
            return;
        }

        String token = authorization.substring("Bearer ".length()).trim();
        try {
            Claims claims = jwtTokenService.parseAccessToken(token);
            String principal = claims.get("email", String.class);
            if (principal == null || principal.isBlank()) {
                principal = claims.get("username", String.class);
            }
            if (principal != null && !principal.isBlank()) {
                UserDetails userDetails = appUserDetailsService.loadUserByUsername(principal);
                UsernamePasswordAuthenticationToken authenticationToken =
                        new UsernamePasswordAuthenticationToken(
                                userDetails,
                                null,
                                userDetails.getAuthorities()
                        );
                authenticationToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                SecurityContextHolder.getContext().setAuthentication(authenticationToken);
            }
        } catch (Exception ignored) {
            SecurityContextHolder.clearContext();
        }

        filterChain.doFilter(request, response);
    }
}
