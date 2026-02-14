package com.example.pos_system.repository;

import com.example.pos_system.entity.ShiftCashEvent;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ShiftCashEventRepo extends JpaRepository<ShiftCashEvent, Long> {
    List<ShiftCashEvent> findByShift_IdOrderByCreatedAtAsc(Long shiftId);
}
