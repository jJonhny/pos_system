package com.devcore.pos_system.api.v1.controller;

import com.devcore.pos_system.api.v1.dto.audit.AuditEventDto;
import com.devcore.pos_system.api.v1.dto.audit.AuditFilterMetaDto;
import com.devcore.pos_system.api.v1.dto.common.ApiEnvelope;
import com.devcore.pos_system.api.v1.dto.common.ApiPageData;
import com.devcore.pos_system.api.v1.service.ApiAuditService;
import com.devcore.pos_system.api.v1.support.ApiTrace;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDate;

@RestController
@RequestMapping("/api/v1/audit")
public class AuditApiV1Controller {
    private static final int DEFAULT_PAGE_SIZE = 50;
    private static final int MAX_PAGE_SIZE = 200;

    private final ApiAuditService apiAuditService;

    public AuditApiV1Controller(ApiAuditService apiAuditService) {
        this.apiAuditService = apiAuditService;
    }

    @GetMapping("/events")
    public ApiEnvelope<ApiPageData<AuditEventDto>> events(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
            @RequestParam(required = false) String user,
            @RequestParam(required = false) String actionType,
            @RequestParam(required = false) String targetType,
            @RequestParam(required = false) String targetId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "50") int size,
            HttpServletRequest request) {
        int safePage = Math.max(0, page);
        int safeSize = normalizePageSize(size);
        Page<AuditEventDto> data = apiAuditService.events(
                from,
                to,
                user,
                actionType,
                targetType,
                targetId,
                PageRequest.of(safePage, safeSize, Sort.by(Sort.Direction.DESC, "timestamp"))
        );
        return ApiEnvelope.success(
                "AUDIT_EVENTS_OK",
                "Audit events fetched successfully.",
                ApiPageData.from(data),
                ApiTrace.resolve(request)
        );
    }

    @GetMapping("/meta")
    public ApiEnvelope<AuditFilterMetaDto> filterMeta(HttpServletRequest request) {
        return ApiEnvelope.success(
                "AUDIT_META_OK",
                "Audit filter metadata fetched successfully.",
                apiAuditService.filterMeta(),
                ApiTrace.resolve(request)
        );
    }

    private int normalizePageSize(int requested) {
        if (requested <= 0) {
            return DEFAULT_PAGE_SIZE;
        }
        return Math.min(requested, MAX_PAGE_SIZE);
    }
}
