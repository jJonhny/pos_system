package com.example.pos_system.config;

import com.example.pos_system.entity.AppUser;
import com.example.pos_system.entity.UserRole;
import com.example.pos_system.repository.AppUserRepo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

@Component
public class UserSeeder implements CommandLineRunner {
    private static final Logger log = LoggerFactory.getLogger(UserSeeder.class);
    private final AppUserRepo appUserRepo;
    private final PasswordEncoder passwordEncoder;

    @Value("${app.seed.admin.username:admin}")
    private String adminUsername;

    @Value("${app.seed.admin.password:admin123}")
    private String adminPassword;

    @Value("${app.seed.cashier.username:cashier}")
    private String cashierUsername;

    @Value("${app.seed.cashier.password:cashier123}")
    private String cashierPassword;

    /**
     * Executes the UserSeeder operation.
     * <p>Return value: A fully initialized UserSeeder instance.</p>
     *
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * @param passwordEncoder Parameter of type {@code PasswordEncoder} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public UserSeeder(AppUserRepo appUserRepo, PasswordEncoder passwordEncoder) {
        this.appUserRepo = appUserRepo;
        this.passwordEncoder = passwordEncoder;
    }

    /**
     * Executes the run operation.
     *
     * @param args Parameter of type {@code String...} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the run operation.
     *
     * @param args Parameter of type {@code String...} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the run operation.
     *
     * @param args Parameter of type {@code String...} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Override
    public void run(String... args) {
        if (appUserRepo.count() > 0) return;

        AppUser admin = new AppUser();
        admin.setUsername(adminUsername);
        admin.setEmail(adminUsername + "@pos.local");
        admin.setPassword(passwordEncoder.encode(adminPassword));
        admin.setRole(UserRole.ADMIN);
        admin.setLanguagePreference("en");
        appUserRepo.save(admin);

        AppUser cashier = new AppUser();
        cashier.setUsername(cashierUsername);
        cashier.setEmail(cashierUsername + "@pos.local");
        cashier.setPassword(passwordEncoder.encode(cashierPassword));
        cashier.setRole(UserRole.CASHIER);
        cashier.setLanguagePreference("en");
        appUserRepo.save(cashier);

        log.warn("Seeded default users: admin='{}', cashier='{}'. Change passwords immediately.",
                adminUsername, cashierUsername);
    }
}
