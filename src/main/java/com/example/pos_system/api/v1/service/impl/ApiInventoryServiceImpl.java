package com.example.pos_system.api.v1.service.impl;

import com.example.pos_system.api.v1.dto.inventory.StockAdjustmentRequest;
import com.example.pos_system.api.v1.dto.inventory.StockAvailabilityDto;
import com.example.pos_system.api.v1.dto.inventory.StockMovementDto;
import com.example.pos_system.api.v1.dto.inventory.StockReceiveRequest;
import com.example.pos_system.api.v1.exception.ApiBadRequestException;
import com.example.pos_system.api.v1.exception.ApiNotFoundException;
import com.example.pos_system.api.v1.mapper.ApiV1Mapper;
import com.example.pos_system.api.v1.service.ApiInventoryService;
import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.StockMovement;
import com.example.pos_system.entity.StockMovementType;
import com.example.pos_system.repository.ProductRepo;
import com.example.pos_system.repository.StockMovementRepo;
import com.example.pos_system.service.StockMovementService;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;

@Service
@Transactional
public class ApiInventoryServiceImpl implements ApiInventoryService {
    private final StockMovementRepo stockMovementRepo;
    private final ProductRepo productRepo;
    private final StockMovementService stockMovementService;
    private final ApiV1Mapper mapper;

    public ApiInventoryServiceImpl(StockMovementRepo stockMovementRepo,
                                   ProductRepo productRepo,
                                   StockMovementService stockMovementService,
                                   ApiV1Mapper mapper) {
        this.stockMovementRepo = stockMovementRepo;
        this.productRepo = productRepo;
        this.stockMovementService = stockMovementService;
        this.mapper = mapper;
    }

    @Override
    @Transactional(readOnly = true)
    public Page<StockMovementDto> listMovements(LocalDate from,
                                                LocalDate to,
                                                Long productId,
                                                StockMovementType type,
                                                Pageable pageable) {
        Specification<StockMovement> specification = buildMovementSpecification(from, to, productId, type);
        return stockMovementRepo.findAll(specification, pageable).map(mapper::toStockMovementDto);
    }

    @Override
    @Transactional(readOnly = true)
    public StockAvailabilityDto getAvailability(Long productId) {
        return mapper.toAvailabilityDto(requireProduct(productId));
    }

    @Override
    public StockAvailabilityDto adjustStock(StockAdjustmentRequest request) {
        if (request.quantity() == null) {
            throw new ApiBadRequestException("quantity is required.");
        }

        Product updated;
        if (request.mode() == StockAdjustmentRequest.AdjustmentMode.DELTA) {
            if (request.quantity() == 0) {
                throw new ApiBadRequestException("delta quantity cannot be zero.");
            }
            updated = stockMovementService.adjustByDelta(
                    request.productId(),
                    request.quantity(),
                    request.unitCost(),
                    request.currency(),
                    StockMovementType.ADJUSTMENT,
                    "API_ADJUST",
                    normalizedReference(request.referenceId(), "adjust"),
                    null,
                    request.reason()
            );
        } else {
            updated = stockMovementService.adjustToTarget(
                    request.productId(),
                    request.quantity(),
                    request.unitCost(),
                    request.currency(),
                    StockMovementType.ADJUSTMENT,
                    "API_ADJUST",
                    normalizedReference(request.referenceId(), "target"),
                    null,
                    request.reason()
            );
        }
        return mapper.toAvailabilityDto(updated);
    }

    @Override
    public StockAvailabilityDto receiveStock(StockReceiveRequest request) {
        Product updated = stockMovementService.recordReceive(
                request.productId(),
                request.quantity(),
                request.unitCost(),
                request.currency(),
                "API_RECEIVE",
                normalizedReference(request.referenceId(), "receive"),
                null,
                request.notes()
        );
        return mapper.toAvailabilityDto(updated);
    }

    private Product requireProduct(Long productId) {
        if (productId == null) {
            throw new ApiBadRequestException("product id is required.");
        }
        return productRepo.findById(productId)
                .orElseThrow(() -> new ApiNotFoundException("product not found."));
    }

    private String normalizedReference(String value, String prefix) {
        if (value != null && !value.isBlank()) {
            return value.trim();
        }
        return prefix + "-" + System.currentTimeMillis();
    }

    private Specification<StockMovement> buildMovementSpecification(LocalDate from,
                                                                    LocalDate to,
                                                                    Long productId,
                                                                    StockMovementType type) {
        return (root, query, cb) -> {
            var predicates = new ArrayList<Predicate>();
            LocalDateTime fromAt = from == null ? null : from.atStartOfDay();
            LocalDateTime toAt = to == null ? null : to.atTime(LocalTime.MAX);
            if (fromAt != null) {
                predicates.add(cb.greaterThanOrEqualTo(root.get("createdAt"), fromAt));
            }
            if (toAt != null) {
                predicates.add(cb.lessThanOrEqualTo(root.get("createdAt"), toAt));
            }
            if (productId != null) {
                predicates.add(cb.equal(root.get("product").get("id"), productId));
            }
            if (type != null) {
                predicates.add(cb.equal(root.get("type"), type));
            }
            return cb.and(predicates.toArray(Predicate[]::new));
        };
    }
}
