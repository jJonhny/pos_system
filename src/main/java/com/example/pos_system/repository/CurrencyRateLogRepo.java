package com.example.pos_system.repository;

import com.example.pos_system.entity.CurrencyRateLog;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface CurrencyRateLogRepo extends JpaRepository<CurrencyRateLog, Long> {
    List<CurrencyRateLog> findTop30ByCurrencyCodeOrderByCreatedAtDesc(String currencyCode);
}
