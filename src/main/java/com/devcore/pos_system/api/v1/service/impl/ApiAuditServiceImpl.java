package com.devcore.pos_system.api.v1.service.impl;

import com.devcore.pos_system.api.v1.dto.audit.AuditEventDto;
import com.devcore.pos_system.api.v1.dto.audit.AuditFilterMetaDto;
import com.devcore.pos_system.api.v1.service.ApiAuditService;
import com.devcore.pos_system.entity.AuditEvent;
import com.devcore.pos_system.repository.AuditEventRepo;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Service
@Transactional(readOnly = true)
public class ApiAuditServiceImpl implements ApiAuditService {
    private final AuditEventRepo auditEventRepo;

    public ApiAuditServiceImpl(AuditEventRepo auditEventRepo) {
        this.auditEventRepo = auditEventRepo;
    }

    @Override
    public Page<AuditEventDto> events(LocalDate from,
                                      LocalDate to,
                                      String user,
                                      String actionType,
                                      String targetType,
                                      String targetId,
                                      Pageable pageable) {
        return auditEventRepo.findAll(buildSpec(from, to, user, actionType, targetType, targetId), pageable)
                .map(this::toDto);
    }

    @Override
    public AuditFilterMetaDto filterMeta() {
        return new AuditFilterMetaDto(
                auditEventRepo.findDistinctActionTypes(),
                auditEventRepo.findDistinctTargetTypes()
        );
    }

    private AuditEventDto toDto(AuditEvent event) {
        return new AuditEventDto(
                event.getId(),
                event.getTimestamp(),
                event.getActorUserId(),
                event.getActorUsername(),
                event.getActionType(),
                event.getTargetType(),
                event.getTargetId(),
                event.getIpAddress(),
                event.getTerminalId(),
                event.getMetadataJson()
        );
    }

    private Specification<AuditEvent> buildSpec(LocalDate from,
                                                LocalDate to,
                                                String user,
                                                String actionType,
                                                String targetType,
                                                String targetId) {
        return (root, query, cb) -> {
            List<Predicate> predicates = new ArrayList<>();
            if (from != null) {
                predicates.add(cb.greaterThanOrEqualTo(root.get("timestamp"), from.atStartOfDay()));
            }
            if (to != null) {
                LocalDateTime toExclusive = to.plusDays(1).atStartOfDay();
                predicates.add(cb.lessThan(root.get("timestamp"), toExclusive));
            }
            if (hasText(user)) {
                predicates.add(cb.like(cb.lower(root.get("actorUsername")), "%" + user.trim().toLowerCase() + "%"));
            }
            if (hasText(actionType)) {
                predicates.add(cb.equal(root.get("actionType"), actionType.trim()));
            }
            if (hasText(targetType)) {
                predicates.add(cb.equal(root.get("targetType"), targetType.trim()));
            }
            if (hasText(targetId)) {
                predicates.add(cb.like(cb.lower(root.get("targetId")), "%" + targetId.trim().toLowerCase() + "%"));
            }
            return cb.and(predicates.toArray(Predicate[]::new));
        };
    }

    private boolean hasText(String value) {
        return value != null && !value.trim().isEmpty();
    }
}
