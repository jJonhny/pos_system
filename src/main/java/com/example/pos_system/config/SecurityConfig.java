package com.example.pos_system.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.example.pos_system.service.AppUserDetailsService;
import com.example.pos_system.config.LoginSuccessHandler;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.web.SecurityFilterChain;

@Configuration
@EnableWebSecurity
public class SecurityConfig {

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http, LoginSuccessHandler loginSuccessHandler) throws Exception {
        http
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers("/login", "/error", "/access-denied").permitAll()
                        .requestMatchers("/users/password").authenticated()
                        .requestMatchers("/users/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasRole('ADMIN') or hasAuthority('PERM_MANAGE_USERS')"))
                        .requestMatchers("/currencies/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasRole('ADMIN')"))
                        .requestMatchers("/admin/audit/**", "/audit-events/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasRole('ADMIN')"))
                        .requestMatchers("/reports/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_VIEW_REPORTS')"))
                        .requestMatchers("/analytics").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_VIEW_ANALYTICS')"))
                        .requestMatchers("/marketing/**", "/pos-setting/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER')"))
                        .requestMatchers("/", "/pos/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER','CASHIER') or hasAuthority('PERM_USE_POS')"))
                        .requestMatchers("/sales/*/receipt").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER','CASHIER') or hasAuthority('PERM_USE_POS')"))
                        .requestMatchers("/inventory/movements/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_INVENTORY_VIEW_MOVEMENTS')"))
                        .requestMatchers("/suppliers/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_SUPPLIERS_MANAGE')"))
                        .requestMatchers("/purchases/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_PURCHASES_MANAGE') or hasAuthority('PERM_RECEIVING_POST')"))
                        .requestMatchers("/commodity", "/products/**", "/categories/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_MANAGE_INVENTORY')"))
                        .requestMatchers("/sales/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_MANAGE_SALES')"))
                        .anyRequest().authenticated()
                )
                .formLogin(form -> form
                        .loginPage("/login")
                        .successHandler(loginSuccessHandler)
                        .permitAll()
                )
                .logout(logout -> logout.logoutSuccessUrl("/login?logout").permitAll())
                .exceptionHandling(e -> e.accessDeniedPage("/access-denied"));
        return http.build();
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new LegacyUpgradingPasswordEncoder();
    }

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration configuration) throws Exception {
        return configuration.getAuthenticationManager();
    }

    @Bean
    public DaoAuthenticationProvider daoAuthenticationProvider(AppUserDetailsService userDetailsService,
                                                               PasswordEncoder passwordEncoder) {
        DaoAuthenticationProvider provider = new DaoAuthenticationProvider(userDetailsService);
        provider.setPasswordEncoder(passwordEncoder);
        provider.setUserDetailsPasswordService(userDetailsService);
        return provider;
    }
}
