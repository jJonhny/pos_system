package com.example.pos_system.repository;

import com.example.pos_system.entity.Currency;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface CurrencyRepo extends JpaRepository<Currency, Long> {
    Optional<Currency> findByCodeIgnoreCase(String code);
    boolean existsByCodeIgnoreCase(String code);
    Optional<Currency> findByBaseTrue();
    List<Currency> findByActiveTrueOrderByCodeAsc();
    List<Currency> findAllByOrderByCodeAsc();
}
