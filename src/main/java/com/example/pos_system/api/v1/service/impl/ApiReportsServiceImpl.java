package com.example.pos_system.api.v1.service.impl;

import com.example.pos_system.api.v1.dto.reports.ReportSaleRowDto;
import com.example.pos_system.api.v1.dto.reports.ReportShiftRowDto;
import com.example.pos_system.api.v1.dto.reports.ReportsSummaryDto;
import com.example.pos_system.api.v1.service.ApiReportsService;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SaleItem;
import com.example.pos_system.entity.SaleStatus;
import com.example.pos_system.entity.Shift;
import com.example.pos_system.entity.ShiftStatus;
import com.example.pos_system.repository.SaleRepo;
import com.example.pos_system.repository.ShiftRepo;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Service
@Transactional(readOnly = true)
public class ApiReportsServiceImpl implements ApiReportsService {
    private final SaleRepo saleRepo;
    private final ShiftRepo shiftRepo;

    public ApiReportsServiceImpl(SaleRepo saleRepo, ShiftRepo shiftRepo) {
        this.saleRepo = saleRepo;
        this.shiftRepo = shiftRepo;
    }

    @Override
    public ReportsSummaryDto summary(LocalDate from, LocalDate to, String cashier, String terminal) {
        List<Sale> sales = filterSales(from, to);
        List<Shift> shifts = filterShifts(from, to, cashier, terminal);

        BigDecimal totalRevenue = sales.stream()
                .map(Sale::getTotal)
                .map(this::safeAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        long refundCount = sales.stream()
                .filter(s -> safeAmount(s.getRefundedTotal()).compareTo(BigDecimal.ZERO) > 0)
                .count();

        BigDecimal refundTotal = sales.stream()
                .map(Sale::getRefundedTotal)
                .map(this::safeAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal averageTicket = sales.isEmpty()
                ? BigDecimal.ZERO
                : totalRevenue.divide(BigDecimal.valueOf(sales.size()), 2, RoundingMode.HALF_UP);

        return new ReportsSummaryDto(
                sales.size(),
                totalRevenue,
                averageTicket,
                refundCount,
                refundTotal,
                shifts.size()
        );
    }

    @Override
    public Page<ReportSaleRowDto> sales(LocalDate from, LocalDate to, Pageable pageable) {
        List<ReportSaleRowDto> rows = filterSales(from, to).stream()
                .map(this::toSaleRow)
                .toList();
        return paginate(rows, pageable);
    }

    @Override
    public Page<ReportShiftRowDto> shifts(LocalDate from, LocalDate to, String cashier, String terminal, Pageable pageable) {
        List<ReportShiftRowDto> rows = filterShifts(from, to, cashier, terminal).stream()
                .map(this::toShiftRow)
                .toList();
        return paginate(rows, pageable);
    }

    private ReportSaleRowDto toSaleRow(Sale sale) {
        return new ReportSaleRowDto(
                sale.getId(),
                sale.getCreatedAt(),
                sale.getCashierUsername(),
                sale.getTerminalId(),
                sale.getPaymentMethod() == null ? null : sale.getPaymentMethod().name(),
                sale.getStatus() == null ? null : sale.getStatus().name(),
                safeAmount(sale.getSubtotal()),
                safeAmount(sale.getDiscount()),
                safeAmount(sale.getTax()),
                safeAmount(sale.getTotal()),
                safeAmount(sale.getRefundedTotal()),
                countSaleItems(sale)
        );
    }

    private ReportShiftRowDto toShiftRow(Shift shift) {
        return new ReportShiftRowDto(
                shift.getId(),
                shift.getCashierUsername(),
                shift.getTerminalId(),
                shift.getStatus() == null ? null : shift.getStatus().name(),
                shift.getOpenedAt(),
                shift.getClosedAt(),
                safeAmount(shift.getTotalSales()),
                safeAmount(shift.getCashTotal()),
                safeAmount(shift.getCardTotal()),
                safeAmount(shift.getQrTotal()),
                safeAmount(shift.getExpectedCash()),
                safeAmount(shift.getClosingCash()),
                safeAmount(shift.getVarianceCash())
        );
    }

    private int countSaleItems(Sale sale) {
        if (sale.getItems() == null) {
            return 0;
        }
        int total = 0;
        for (SaleItem item : sale.getItems()) {
            if (item == null || item.getQty() == null) {
                continue;
            }
            int returned = item.getReturnedQty() == null ? 0 : item.getReturnedQty();
            total += Math.max(0, item.getQty() - returned);
        }
        return total;
    }

    private List<Sale> filterSales(LocalDate from, LocalDate to) {
        List<Sale> sales = saleRepo.findAll(Sort.by(Sort.Direction.DESC, "createdAt"));
        List<Sale> filtered = new ArrayList<>();
        for (Sale sale : sales) {
            if (sale == null || sale.getCreatedAt() == null) {
                continue;
            }
            if (!isReportableStatus(sale.getStatus())) {
                continue;
            }
            LocalDate date = sale.getCreatedAt().toLocalDate();
            if (from != null && date.isBefore(from)) {
                continue;
            }
            if (to != null && date.isAfter(to)) {
                continue;
            }
            filtered.add(sale);
        }
        return filtered;
    }

    private boolean isReportableStatus(SaleStatus status) {
        return status == SaleStatus.PAID
                || status == SaleStatus.PARTIALLY_RETURNED
                || status == SaleStatus.RETURNED;
    }

    private List<Shift> filterShifts(LocalDate from, LocalDate to, String cashier, String terminal) {
        List<Shift> shifts = shiftRepo.findAll(Sort.by(Sort.Direction.DESC, "openedAt"));
        String cashierFilter = normalize(cashier);
        String terminalFilter = normalize(terminal);

        List<Shift> filtered = new ArrayList<>();
        for (Shift shift : shifts) {
            if (shift == null || shift.getStatus() != ShiftStatus.CLOSED) {
                continue;
            }
            if (!matches(shift.getCashierUsername(), cashierFilter)) {
                continue;
            }
            if (!matches(shift.getTerminalId(), terminalFilter)) {
                continue;
            }
            LocalDate date = referenceDate(shift);
            if (date == null) {
                continue;
            }
            if (from != null && date.isBefore(from)) {
                continue;
            }
            if (to != null && date.isAfter(to)) {
                continue;
            }
            filtered.add(shift);
        }
        return filtered;
    }

    private LocalDate referenceDate(Shift shift) {
        LocalDateTime closedAt = shift.getClosedAt();
        if (closedAt != null) {
            return closedAt.toLocalDate();
        }
        if (shift.getOpenedAt() != null) {
            return shift.getOpenedAt().toLocalDate();
        }
        return null;
    }

    private String normalize(String value) {
        if (value == null) {
            return null;
        }
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private boolean matches(String value, String filter) {
        if (filter == null) {
            return true;
        }
        return value != null && value.equalsIgnoreCase(filter);
    }

    private BigDecimal safeAmount(BigDecimal amount) {
        return amount == null ? BigDecimal.ZERO : amount;
    }

    private <T> Page<T> paginate(List<T> rows, Pageable pageable) {
        int total = rows.size();
        int offset = (int) pageable.getOffset();
        if (offset >= total) {
            return new PageImpl<>(List.of(), pageable, total);
        }
        int end = Math.min(offset + pageable.getPageSize(), total);
        return new PageImpl<>(rows.subList(offset, end), pageable, total);
    }
}
