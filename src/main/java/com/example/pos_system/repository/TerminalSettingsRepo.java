package com.example.pos_system.repository;

import com.example.pos_system.entity.TerminalSettings;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface TerminalSettingsRepo extends JpaRepository<TerminalSettings, Long> {
    Optional<TerminalSettings> findByTerminalIdIgnoreCase(String terminalId);

    List<TerminalSettings> findAllByOrderByNameAscTerminalIdAsc();
}
