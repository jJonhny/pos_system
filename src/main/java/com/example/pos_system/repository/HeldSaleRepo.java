package com.example.pos_system.repository;

import com.example.pos_system.entity.HeldSale;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface HeldSaleRepo extends JpaRepository<HeldSale, Long> {
    List<HeldSale> findByCashierUsernameOrderByCreatedAtDesc(String cashierUsername);
}
