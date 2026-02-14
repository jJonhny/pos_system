package com.example.pos_system.repository;

import com.example.pos_system.entity.StockMovement;
import com.example.pos_system.entity.StockMovementType;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.time.LocalDateTime;
import java.util.List;

public interface StockMovementRepo extends JpaRepository<StockMovement, Long>, JpaSpecificationExecutor<StockMovement> {
    @EntityGraph(attributePaths = {"product", "product.category"})
    List<StockMovement> findByCreatedAtBetweenOrderByCreatedAtDesc(LocalDateTime from, LocalDateTime to);

    long countByType(StockMovementType type);
}
