package com.devcore.pos_system.api.v1.controller;

import com.devcore.pos_system.api.v1.dto.common.ApiEnvelope;
import com.devcore.pos_system.api.v1.dto.common.ApiPageData;
import com.devcore.pos_system.api.v1.dto.reports.ReportSaleRowDto;
import com.devcore.pos_system.api.v1.dto.reports.ReportShiftRowDto;
import com.devcore.pos_system.api.v1.dto.reports.ReportsSummaryDto;
import com.devcore.pos_system.api.v1.service.ApiReportsService;
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
@RequestMapping("/api/v1/reports")
public class ReportsApiV1Controller {
    private static final int DEFAULT_PAGE_SIZE = 20;
    private static final int MAX_PAGE_SIZE = 200;

    private final ApiReportsService apiReportsService;

    public ReportsApiV1Controller(ApiReportsService apiReportsService) {
        this.apiReportsService = apiReportsService;
    }

    @GetMapping("/summary")
    public ApiEnvelope<ReportsSummaryDto> summary(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
            @RequestParam(required = false) String cashier,
            @RequestParam(required = false) String terminal,
            HttpServletRequest request) {
        return ApiEnvelope.success(
                "REPORTS_SUMMARY_OK",
                "Report summary fetched successfully.",
                apiReportsService.summary(from, to, cashier, terminal),
                ApiTrace.resolve(request)
        );
    }

    @GetMapping("/sales")
    public ApiEnvelope<ApiPageData<ReportSaleRowDto>> sales(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            HttpServletRequest request) {
        int safePage = Math.max(0, page);
        int safeSize = normalizePageSize(size);
        Page<ReportSaleRowDto> data = apiReportsService.sales(
                from,
                to,
                PageRequest.of(safePage, safeSize, Sort.by(Sort.Direction.DESC, "createdAt"))
        );
        return ApiEnvelope.success(
                "REPORTS_SALES_OK",
                "Sales report fetched successfully.",
                ApiPageData.from(data),
                ApiTrace.resolve(request)
        );
    }

    @GetMapping("/shifts")
    public ApiEnvelope<ApiPageData<ReportShiftRowDto>> shifts(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate from,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate to,
            @RequestParam(required = false) String cashier,
            @RequestParam(required = false) String terminal,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            HttpServletRequest request) {
        int safePage = Math.max(0, page);
        int safeSize = normalizePageSize(size);
        Page<ReportShiftRowDto> data = apiReportsService.shifts(
                from,
                to,
                cashier,
                terminal,
                PageRequest.of(safePage, safeSize, Sort.by(Sort.Direction.DESC, "openedAt"))
        );
        return ApiEnvelope.success(
                "REPORTS_SHIFTS_OK",
                "Shift report fetched successfully.",
                ApiPageData.from(data),
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
