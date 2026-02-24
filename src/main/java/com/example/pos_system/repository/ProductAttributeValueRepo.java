package com.example.pos_system.repository;

import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.ProductAttributeValue;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ProductAttributeValueRepo extends JpaRepository<ProductAttributeValue, Long> {
    List<ProductAttributeValue> findByProductAndActiveTrueOrderByGroup_SortOrderAscValue_SortOrderAsc(Product product);
    List<ProductAttributeValue> findByProductOrderByGroup_SortOrderAscValue_SortOrderAsc(Product product);
    void deleteByProduct(Product product);
}
