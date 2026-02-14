package com.example.pos_system.repository;

import com.example.pos_system.entity.Shift;
import com.example.pos_system.entity.ShiftStatus;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

public interface ShiftRepo extends JpaRepository<Shift, Long> {
    Optional<Shift> findByCashierUsernameAndStatus(String cashierUsername, ShiftStatus status);
    Optional<Shift> findByCashierUsernameAndTerminalIdAndStatus(String cashierUsername, String terminalId, ShiftStatus status);
    List<Shift> findByCashierUsernameOrderByOpenedAtDesc(String cashierUsername);
    List<Shift> findByStatusOrderByOpenedAtDesc(ShiftStatus status);
    List<Shift> findByOpenedAtBetweenOrderByOpenedAtDesc(LocalDateTime from, LocalDateTime to);
}
