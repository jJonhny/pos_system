package com.example.pos_system.repository;

import com.example.pos_system.entity.Sale;
import com.example.pos_system.entity.SaleStatus;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface SaleRepo extends JpaRepository<Sale, Long> {
    List<Sale> findByShift_Id(Long shiftId);
    List<Sale> findByCustomer_Id(Long customerId);
    boolean existsByCustomer_IdAndStatusNot(Long customerId, SaleStatus status);

    @EntityGraph(attributePaths = {"items", "items.product", "customer", "shift"})
    @Query("select s from Sale s where s.id = :id")
    Optional<Sale> findByIdForReceipt(@Param("id") Long id);
}
