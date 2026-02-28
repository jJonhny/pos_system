package com.devcore.pos_system.api.v1.service;

import com.devcore.pos_system.api.v1.dto.audit.AuditEventDto;
import com.devcore.pos_system.api.v1.dto.audit.AuditFilterMetaDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.time.LocalDate;

public interface ApiAuditService {
    Page<AuditEventDto> events(LocalDate from,
                               LocalDate to,
                               String user,
                               String actionType,
                               String targetType,
                               String targetId,
                               Pageable pageable);

    AuditFilterMetaDto filterMeta();
}
