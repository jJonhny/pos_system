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

    public UserSeeder(AppUserRepo appUserRepo, PasswordEncoder passwordEncoder) {
        this.appUserRepo = appUserRepo;
        this.passwordEncoder = passwordEncoder;
    }

    @Override
    public void run(String... args) {
        if (appUserRepo.count() > 0) return;

        AppUser admin = new AppUser();
        admin.setUsername(adminUsername);
        admin.setPassword(passwordEncoder.encode(adminPassword));
        admin.setRole(UserRole.ADMIN);
        admin.setLanguagePreference("en");
        appUserRepo.save(admin);

        AppUser cashier = new AppUser();
        cashier.setUsername(cashierUsername);
        cashier.setPassword(passwordEncoder.encode(cashierPassword));
        cashier.setRole(UserRole.CASHIER);
        cashier.setLanguagePreference("en");
        appUserRepo.save(cashier);

        log.warn("Seeded default users: admin='{}', cashier='{}'. Change passwords immediately.",
                adminUsername, cashierUsername);
    }
}
