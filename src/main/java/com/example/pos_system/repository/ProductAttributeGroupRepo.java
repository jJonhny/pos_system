package com.example.pos_system.repository;

import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.ProductAttributeGroup;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ProductAttributeGroupRepo extends JpaRepository<ProductAttributeGroup, Long> {
    List<ProductAttributeGroup> findByProductOrderBySortOrderAscIdAsc(Product product);
    void deleteByProduct(Product product);
}
