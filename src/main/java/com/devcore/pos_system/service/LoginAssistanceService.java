package com.devcore.pos_system.service;

import com.devcore.pos_system.repository.AppUserRepo;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.LinkedHashMap;
import java.util.Map;

@Service
@Transactional
public class LoginAssistanceService {
    private final boolean ssoEnabled;
    private final String ssoRegistrationId;
    private final ObjectProvider<ClientRegistrationRepository> clientRegistrationRepositoryProvider;
    private final AppUserRepo appUserRepo;
    private final AuditEventService auditEventService;

    /**
     * Executes the LoginAssistanceService operation.
     * <p>Return value: A fully initialized LoginAssistanceService instance.</p>
     *
     * @param ssoEnabled Parameter of type {@code boolean} used by this operation.
     * @param ssoRegistrationId Parameter of type {@code String} used by this operation.
     * @param clientRegistrationRepositoryProvider Parameter of type {@code ObjectProvider<ClientRegistrationRepository>} used by this operation.
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public LoginAssistanceService(@Value("${app.auth.sso.enabled:false}") boolean ssoEnabled,
                                  @Value("${app.auth.sso.registration-id:corp}") String ssoRegistrationId,
                                  ObjectProvider<ClientRegistrationRepository> clientRegistrationRepositoryProvider,
                                  AppUserRepo appUserRepo,
                                  AuditEventService auditEventService) {
        this.ssoEnabled = ssoEnabled;
        this.ssoRegistrationId = ssoRegistrationId == null || ssoRegistrationId.isBlank()
                ? "corp"
                : ssoRegistrationId.trim();
        this.clientRegistrationRepositoryProvider = clientRegistrationRepositoryProvider;
        this.appUserRepo = appUserRepo;
        this.auditEventService = auditEventService;
    }

    /**
     * Executes the requestPasswordHelp operation.
     *
     * @param rawUsername Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void requestPasswordHelp(String rawUsername) {
        String username = normalize(rawUsername);
        boolean knownUser = username != null && appUserRepo.existsByUsernameIgnoreCase(username);

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("usernameProvided", username != null);
        metadata.put("knownUser", knownUser);
        if (username != null) {
            metadata.put("username", username);
        }

        auditEventService.record(
                "LOGIN_PASSWORD_HELP_REQUEST",
                "AUTH",
                username,
                null,
                null,
                metadata
        );
    }

    /**
     * Executes the requestSsoSignIn operation.
     *
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public boolean requestSsoSignIn() {
        boolean ready = isSsoReady();
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("status", ready ? "redirecting" : "not_configured");
        metadata.put("registrationId", ssoRegistrationId);
        auditEventService.record(
                "LOGIN_SSO_REQUEST",
                "AUTH",
                null,
                null,
                null,
                metadata
        );
        return ready;
    }

    /**
     * Executes the isSsoReady operation.
     *
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public boolean isSsoReady() {
        if (!ssoEnabled) {
            return false;
        }
        ClientRegistrationRepository repository = clientRegistrationRepositoryProvider.getIfAvailable();
        if (repository == null) {
            return false;
        }
        return repository.findByRegistrationId(ssoRegistrationId) != null;
    }

    /**
     * Executes the ssoAuthorizationPath operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String ssoAuthorizationPath() {
        return "/oauth2/authorization/" + ssoRegistrationId;
    }

    /**
     * Executes the normalize operation.
     *
     * @param raw Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalize(String raw) {
        if (raw == null) {
            return null;
        }
        String normalized = raw.trim();
        return normalized.isEmpty() ? null : normalized;
    }
}
