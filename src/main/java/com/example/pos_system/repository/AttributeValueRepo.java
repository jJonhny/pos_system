package com.example.pos_system.repository;

import com.example.pos_system.entity.AttributeGroup;
import com.example.pos_system.entity.AttributeValue;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

public interface AttributeValueRepo extends JpaRepository<AttributeValue, Long> {
    Optional<AttributeValue> findByGroupAndCodeIgnoreCase(AttributeGroup group, String code);
    List<AttributeValue> findByGroupOrderBySortOrderAscDisplayNameAsc(AttributeGroup group);
    List<AttributeValue> findByIdIn(Collection<Long> ids);
}
