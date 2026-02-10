package com.example.pos_system.repository;

import com.example.pos_system.entity.Sale;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface SaleRepo extends JpaRepository<Sale, Long> {
    List<Sale> findByShift_Id(Long shiftId);
    List<Sale> findByCustomer_Id(Long customerId);
}
