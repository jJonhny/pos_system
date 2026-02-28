package com.devcore.pos_system.api.v1.dto.audit;

import java.time.LocalDateTime;

public record AuditEventDto(
        Long id,
        LocalDateTime timestamp,
        Long actorUserId,
        String actorUsername,
        String actionType,
        String targetType,
        String targetId,
        String ipAddress,
        String terminalId,
        String metadataJson
) {
}
