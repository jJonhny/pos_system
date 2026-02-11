package com.example.pos_system.repository;

import com.example.pos_system.entity.UserAuditLog;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface UserAuditLogRepo extends JpaRepository<UserAuditLog, Long> {
    List<UserAuditLog> findTop50ByOrderByCreatedAtDesc();
}
