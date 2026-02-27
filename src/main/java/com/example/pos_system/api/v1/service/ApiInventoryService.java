package com.example.pos_system.api.v1.service;

import com.example.pos_system.api.v1.dto.inventory.StockAdjustmentRequest;
import com.example.pos_system.api.v1.dto.inventory.StockAvailabilityDto;
import com.example.pos_system.api.v1.dto.inventory.StockMovementDto;
import com.example.pos_system.api.v1.dto.inventory.StockReceiveRequest;
import com.example.pos_system.entity.StockMovementType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.time.LocalDate;

public interface ApiInventoryService {
    Page<StockMovementDto> listMovements(LocalDate from,
                                         LocalDate to,
                                         Long productId,
                                         StockMovementType type,
                                         Pageable pageable);

    StockAvailabilityDto getAvailability(Long productId);

    StockAvailabilityDto adjustStock(StockAdjustmentRequest request);

    StockAvailabilityDto receiveStock(StockReceiveRequest request);
}
