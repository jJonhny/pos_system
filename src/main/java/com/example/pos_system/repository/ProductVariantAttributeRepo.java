package com.example.pos_system.repository;

import com.example.pos_system.entity.ProductVariant;
import com.example.pos_system.entity.ProductVariantAttribute;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ProductVariantAttributeRepo extends JpaRepository<ProductVariantAttribute, Long> {
    List<ProductVariantAttribute> findByVariantOrderByGroup_SortOrderAsc(ProductVariant variant);
    void deleteByVariant(ProductVariant variant);
}
