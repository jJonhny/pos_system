package com.example.pos_system.repository;

import com.example.pos_system.entity.AttributeGroup;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface AttributeGroupRepo extends JpaRepository<AttributeGroup, Long> {
    Optional<AttributeGroup> findByCodeIgnoreCase(String code);
    List<AttributeGroup> findAllByOrderBySortOrderAscNameAsc();
}
