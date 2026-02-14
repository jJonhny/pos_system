package com.example.pos_system.controller;

import com.example.pos_system.entity.AuditEvent;
import com.example.pos_system.repository.AuditEventRepo;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Controller
@RequestMapping({"/admin/audit", "/audit-events"})
public class AuditEventsController {
    private static final int PAGE_SIZE = 50;
    private final AuditEventRepo auditEventRepo;

    public AuditEventsController(AuditEventRepo auditEventRepo) {
        this.auditEventRepo = auditEventRepo;
    }

    @GetMapping
    public String list(@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
                       @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
                       @RequestParam(required = false) String user,
                       @RequestParam(required = false) String actionType,
                       @RequestParam(required = false) String targetType,
                       @RequestParam(required = false) String targetId,
                       @RequestParam(defaultValue = "0") int page,
                       @RequestHeader(value = "HX-Request", required = false) String hxRequest,
                       HttpServletRequest request,
                       Model model) {
        int pageNum = Math.max(0, page);
        Page<AuditEvent> events = auditEventRepo.findAll(
                buildSpec(from, to, user, actionType, targetType, targetId),
                PageRequest.of(pageNum, PAGE_SIZE, Sort.by(Sort.Direction.DESC, "timestamp"))
        );
        String basePath = resolveBasePath(request);
        model.addAttribute("events", events.getContent());
        model.addAttribute("actionTypes", auditEventRepo.findDistinctActionTypes());
        model.addAttribute("targetTypes", auditEventRepo.findDistinctTargetTypes());
        model.addAttribute("from", from);
        model.addAttribute("to", to);
        model.addAttribute("user", user);
        model.addAttribute("actionType", actionType);
        model.addAttribute("targetType", targetType);
        model.addAttribute("targetId", targetId);
        model.addAttribute("page", events.getNumber());
        model.addAttribute("totalPages", Math.max(1, events.getTotalPages()));
        model.addAttribute("hasNext", events.hasNext());
        model.addAttribute("hasPrev", events.hasPrevious());
        model.addAttribute("nextPage", events.getNumber() + 1);
        model.addAttribute("prevPage", Math.max(0, events.getNumber() - 1));
        model.addAttribute("basePath", basePath);
        return isHtmx(hxRequest) ? "admin/audit/fragments :: results" : "admin/audit/index";
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
            if (user != null && !user.isBlank()) {
                predicates.add(cb.like(cb.lower(root.get("actorUsername")), "%" + user.trim().toLowerCase() + "%"));
            }
            if (actionType != null && !actionType.isBlank()) {
                predicates.add(cb.equal(root.get("actionType"), actionType.trim()));
            }
            if (targetType != null && !targetType.isBlank()) {
                predicates.add(cb.equal(root.get("targetType"), targetType.trim()));
            }
            if (targetId != null && !targetId.isBlank()) {
                predicates.add(cb.like(cb.lower(root.get("targetId")), "%" + targetId.trim().toLowerCase() + "%"));
            }
            return cb.and(predicates.toArray(new Predicate[0]));
        };
    }

    private boolean isHtmx(String hxRequest) {
        return hxRequest != null && !hxRequest.isBlank();
    }

    private String resolveBasePath(HttpServletRequest request) {
        if (request == null) return "/admin/audit";
        String uri = request.getRequestURI();
        if (uri == null || uri.isBlank()) return "/admin/audit";
        return uri.startsWith("/audit-events") ? "/audit-events" : "/admin/audit";
    }
}
