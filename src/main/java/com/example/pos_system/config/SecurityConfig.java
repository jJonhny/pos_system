package com.example.pos_system.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.example.pos_system.service.AppUserDetailsService;
import com.example.pos_system.service.SsoAuthenticationService;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationFailureHandler;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.core.context.SecurityContextHolder;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;

@Configuration
@EnableWebSecurity
public class SecurityConfig {
    @Value("${app.auth.sso.enabled:false}")
    private boolean ssoEnabled;

    @Value("${app.auth.sso.registration-id:corp}")
    private String ssoRegistrationId;

    @Value("${app.security.remember-me.key}")
    private String rememberMeKey;

    /**
     * Executes the securityFilterChain operation.
     *
     * @param http Parameter of type {@code HttpSecurity} used by this operation.
     * @param loginSuccessHandler Parameter of type {@code LoginSuccessHandler} used by this operation.
     * @param loginFailureHandler Parameter of type {@code LoginFailureHandler} used by this operation.
     * @param appUserDetailsService Parameter of type {@code AppUserDetailsService} used by this operation.
     * @param ssoAuthenticationService Parameter of type {@code SsoAuthenticationService} used by this operation.
     * @param clientRegistrationRepositoryProvider Parameter of type {@code ObjectProvider<ClientRegistrationRepository>} used by this operation.
     * @param jwtAuthenticationFilter Parameter of type {@code JwtAuthenticationFilter} used by this operation.
     * @return {@code SecurityFilterChain} Result produced by this operation.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the securityFilterChain operation.
     *
     * @param http Parameter of type {@code HttpSecurity} used by this operation.
     * @param loginSuccessHandler Parameter of type {@code LoginSuccessHandler} used by this operation.
     * @param loginFailureHandler Parameter of type {@code LoginFailureHandler} used by this operation.
     * @param appUserDetailsService Parameter of type {@code AppUserDetailsService} used by this operation.
     * @param ssoAuthenticationService Parameter of type {@code SsoAuthenticationService} used by this operation.
     * @param clientRegistrationRepositoryProvider Parameter of type {@code ObjectProvider<ClientRegistrationRepository>} used by this operation.
     * @param jwtAuthenticationFilter Parameter of type {@code JwtAuthenticationFilter} used by this operation.
     * @return {@code SecurityFilterChain} Result produced by this operation.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the securityFilterChain operation.
     *
     * @param http Parameter of type {@code HttpSecurity} used by this operation.
     * @param loginSuccessHandler Parameter of type {@code LoginSuccessHandler} used by this operation.
     * @param loginFailureHandler Parameter of type {@code LoginFailureHandler} used by this operation.
     * @param appUserDetailsService Parameter of type {@code AppUserDetailsService} used by this operation.
     * @param ssoAuthenticationService Parameter of type {@code SsoAuthenticationService} used by this operation.
     * @param clientRegistrationRepositoryProvider Parameter of type {@code ObjectProvider<ClientRegistrationRepository>} used by this operation.
     * @param jwtAuthenticationFilter Parameter of type {@code JwtAuthenticationFilter} used by this operation.
     * @return {@code SecurityFilterChain} Result produced by this operation.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http,
                                                   LoginSuccessHandler loginSuccessHandler,
                                                   LoginFailureHandler loginFailureHandler,
                                                   AppUserDetailsService appUserDetailsService,
                                                   SsoAuthenticationService ssoAuthenticationService,
                                                   ObjectProvider<ClientRegistrationRepository> clientRegistrationRepositoryProvider,
                                                   JwtAuthenticationFilter jwtAuthenticationFilter) throws Exception {
        http
                .cors(Customizer.withDefaults())
                .csrf(csrf -> csrf.ignoringRequestMatchers(
                        "/api/v1/**",
                        "/api/v1/auth/register",
                        "/api/v1/auth/login",
                        "/api/v1/auth/verify-otp",
                        "/dev-sso/**"))
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers("/login", "/login/forgot-password", "/login/sso", "/error", "/access-denied").permitAll()
                        .requestMatchers("/oauth2/**", "/login/oauth2/**").permitAll()
                        .requestMatchers("/dev-sso/**").permitAll()
                        .requestMatchers("/api/v1/auth/register", "/api/v1/auth/login", "/api/v1/auth/verify-otp").permitAll()
                        .requestMatchers("/api/v1/auth/me").authenticated()
                        .requestMatchers("/api/v1/users/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasRole('ADMIN') or hasAuthority('PERM_MANAGE_USERS')"))
                        .requestMatchers("/api/v1/products/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_MANAGE_INVENTORY')"))
                        .requestMatchers("/api/v1/inventory/products/*/availability").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER','CASHIER') or hasAuthority('PERM_USE_POS')"))
                        .requestMatchers("/api/v1/inventory/movements/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_INVENTORY_VIEW_MOVEMENTS')"))
                        .requestMatchers("/api/v1/inventory/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_MANAGE_INVENTORY')"))
                        .requestMatchers("/api/v1/reports/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_VIEW_REPORTS')"))
                        .requestMatchers("/api/v1/audit/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasRole('ADMIN')"))
                        .requestMatchers("/support/**", "/legal/**").permitAll()
                        .requestMatchers("/css/**", "/js/**", "/images/**", "/uploads/**", "/favicon.ico").permitAll()
                        .requestMatchers("/users/password", "/users/password/**").authenticated()
                        .requestMatchers("/users/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasRole('ADMIN') or hasAuthority('PERM_MANAGE_USERS')"))
                        .requestMatchers("/currencies/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasRole('ADMIN')"))
                        .requestMatchers("/admin/audit/**", "/audit-events/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasRole('ADMIN')"))
                        .requestMatchers("/reports/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_VIEW_REPORTS')"))
                        .requestMatchers("/analytics", "/analytics/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_VIEW_ANALYTICS')"))
                        .requestMatchers("/marketing/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER')"))
                        .requestMatchers("/pos-setting/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasRole('ADMIN') or hasAuthority('PERM_POS_TERMINAL_SETTINGS')"))
                        .requestMatchers("/pos/checkout/*/print").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER','CASHIER') or hasAuthority('PERM_POS_PRINT') or hasAuthority('PERM_USE_POS')"))
                        .requestMatchers("/pos/drawer/open").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER','CASHIER') or hasAuthority('PERM_POS_DRAWER_OPEN') or hasAuthority('PERM_USE_POS')"))
                        .requestMatchers("/api/v1/pos/pricing/quote").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER','CASHIER') or hasAuthority('PERM_USE_POS')"))
                        .requestMatchers("/api/v1/**").access(new org.springframework.security.web.access.expression.WebExpressionAuthorizationManager(
                                "hasAnyRole('ADMIN','MANAGER') or hasAuthority('PERM_MANAGE_INVENTORY')"))
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
                        .failureHandler(loginFailureHandler)
                        .permitAll()
                );

        ClientRegistrationRepository clientRegistrationRepository = clientRegistrationRepositoryProvider.getIfAvailable();
        if (isSsoReady(clientRegistrationRepository)) {
            http.oauth2Login(oauth2 -> oauth2
                    .loginPage("/login")
                    .successHandler((request, response, authentication) ->
                            handleSsoSuccess(request, response, authentication, ssoAuthenticationService))
                    .failureHandler(new SimpleUrlAuthenticationFailureHandler("/login?ssoUnavailable=1"))
            );
        }

        http
                .rememberMe(remember -> remember
                        .rememberMeParameter("remember-me")
                        .rememberMeCookieName("POS_REMEMBER_ME")
                        .tokenValiditySeconds(60 * 60 * 24 * 14)
                        .key(rememberMeKey)
                        .userDetailsService(appUserDetailsService)
                )
                .addFilterBefore(jwtAuthenticationFilter, UsernamePasswordAuthenticationFilter.class)
                .logout(logout -> logout.logoutSuccessUrl("/login?logout").permitAll())
                .exceptionHandling(e -> e.accessDeniedPage("/access-denied"));
        return http.build();
    }

    /**
     * Executes the passwordEncoder operation.
     *
     * @return {@code PasswordEncoder} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the passwordEncoder operation.
     *
     * @return {@code PasswordEncoder} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the passwordEncoder operation.
     *
     * @return {@code PasswordEncoder} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Bean
    public PasswordEncoder passwordEncoder() {
        return new LegacyUpgradingPasswordEncoder();
    }

    /**
     * Executes the authenticationManager operation.
     *
     * @param configuration Parameter of type {@code AuthenticationConfiguration} used by this operation.
     * @return {@code AuthenticationManager} Result produced by this operation.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the authenticationManager operation.
     *
     * @param configuration Parameter of type {@code AuthenticationConfiguration} used by this operation.
     * @return {@code AuthenticationManager} Result produced by this operation.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the authenticationManager operation.
     *
     * @param configuration Parameter of type {@code AuthenticationConfiguration} used by this operation.
     * @return {@code AuthenticationManager} Result produced by this operation.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration configuration) throws Exception {
        return configuration.getAuthenticationManager();
    }

    /**
     * Executes the daoAuthenticationProvider operation.
     *
     * @param userDetailsService Parameter of type {@code AppUserDetailsService} used by this operation.
     * @param passwordEncoder Parameter of type {@code PasswordEncoder} used by this operation.
     * @return {@code DaoAuthenticationProvider} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the daoAuthenticationProvider operation.
     *
     * @param userDetailsService Parameter of type {@code AppUserDetailsService} used by this operation.
     * @param passwordEncoder Parameter of type {@code PasswordEncoder} used by this operation.
     * @return {@code DaoAuthenticationProvider} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the daoAuthenticationProvider operation.
     *
     * @param userDetailsService Parameter of type {@code AppUserDetailsService} used by this operation.
     * @param passwordEncoder Parameter of type {@code PasswordEncoder} used by this operation.
     * @return {@code DaoAuthenticationProvider} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Bean
    public DaoAuthenticationProvider daoAuthenticationProvider(AppUserDetailsService userDetailsService,
                                                               PasswordEncoder passwordEncoder) {
        DaoAuthenticationProvider provider = new DaoAuthenticationProvider(userDetailsService);
        provider.setPasswordEncoder(passwordEncoder);
        provider.setUserDetailsPasswordService(userDetailsService);
        return provider;
    }

    /**
     * Executes the isSsoReady operation.
     *
     * @param clientRegistrationRepository Parameter of type {@code ClientRegistrationRepository} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean isSsoReady(ClientRegistrationRepository clientRegistrationRepository) {
        if (!ssoEnabled || clientRegistrationRepository == null) {
            return false;
        }
        String registrationId = (ssoRegistrationId == null || ssoRegistrationId.isBlank()) ? "corp" : ssoRegistrationId.trim();
        return clientRegistrationRepository.findByRegistrationId(registrationId) != null;
    }

    /**
     * Executes the handleSsoSuccess operation.
     *
     * @param request Parameter of type {@code HttpServletRequest} used by this operation.
     * @param response Parameter of type {@code HttpServletResponse} used by this operation.
     * @param authentication Parameter of type {@code org.springframework.security.core.Authentication} used by this operation.
     * @param ssoAuthenticationService Parameter of type {@code SsoAuthenticationService} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws IOException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void handleSsoSuccess(HttpServletRequest request,
                                  HttpServletResponse response,
                                  org.springframework.security.core.Authentication authentication,
                                  SsoAuthenticationService ssoAuthenticationService) throws IOException {
        try {
            SecurityContextHolder.getContext().setAuthentication(ssoAuthenticationService.completeLogin(authentication));
            response.sendRedirect("/");
        } catch (AuthenticationException ex) {
            SecurityContextHolder.clearContext();
            response.sendRedirect("/login?ssoUnavailable=1");
        }
    }
}
