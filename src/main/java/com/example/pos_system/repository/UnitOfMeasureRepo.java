package com.example.pos_system.repository;

import com.example.pos_system.entity.UnitOfMeasure;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface UnitOfMeasureRepo extends JpaRepository<UnitOfMeasure, Long> {
    Optional<UnitOfMeasure> findByCodeIgnoreCase(String code);
}
