package com.example.pos_system.repository;

import com.example.pos_system.entity.CustomerGroup;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface CustomerGroupRepo extends JpaRepository<CustomerGroup, Long> {
    Optional<CustomerGroup> findByCodeIgnoreCase(String code);
}
