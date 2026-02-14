package com.example.pos_system.repository;

import com.example.pos_system.entity.GoodsReceipt;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface GoodsReceiptRepo extends JpaRepository<GoodsReceipt, Long> {
    @EntityGraph(attributePaths = {"purchaseOrder", "purchaseOrder.supplier", "items", "items.product"})
    List<GoodsReceipt> findAllByOrderByReceivedAtDesc();

    @EntityGraph(attributePaths = {"purchaseOrder", "purchaseOrder.supplier", "items", "items.product"})
    Optional<GoodsReceipt> findDetailedById(Long id);
}
