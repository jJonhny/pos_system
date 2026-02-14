package com.example.pos_system.repository;

import com.example.pos_system.entity.AuditEvent;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface AuditEventRepo extends JpaRepository<AuditEvent, Long>, JpaSpecificationExecutor<AuditEvent> {
    Optional<AuditEvent> findTopByActionTypeOrderByTimestampDesc(String actionType);

    @Query("select distinct a.actionType from AuditEvent a order by a.actionType asc")
    List<String> findDistinctActionTypes();

    @Query("select distinct a.targetType from AuditEvent a order by a.targetType asc")
    List<String> findDistinctTargetTypes();

    long countByActionType(String actionType);
}
