package com.example.pos_system.repository;

import com.example.pos_system.entity.Supplier;
import com.example.pos_system.entity.SupplierStatus;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface SupplierRepo extends JpaRepository<Supplier, Long> {
    List<Supplier> findByStatus(SupplierStatus status, Sort sort);
    List<Supplier> findByNameContainingIgnoreCase(String q, Sort sort);
    List<Supplier> findByNameContainingIgnoreCaseAndStatus(String q, SupplierStatus status, Sort sort);
}
