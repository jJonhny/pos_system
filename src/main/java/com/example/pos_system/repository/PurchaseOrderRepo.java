package com.example.pos_system.repository;

import com.example.pos_system.entity.PurchaseOrder;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface PurchaseOrderRepo extends JpaRepository<PurchaseOrder, Long> {
    @EntityGraph(attributePaths = {"supplier"})
    List<PurchaseOrder> findAllByOrderByCreatedAtDesc();

    @EntityGraph(attributePaths = {"supplier", "items", "items.product"})
    Optional<PurchaseOrder> findDetailedById(Long id);
}
