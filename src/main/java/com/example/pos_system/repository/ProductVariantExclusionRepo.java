package com.example.pos_system.repository;

import com.example.pos_system.entity.Product;
import com.example.pos_system.entity.ProductVariantExclusion;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface ProductVariantExclusionRepo extends JpaRepository<ProductVariantExclusion, Long> {
    List<ProductVariantExclusion> findByProductAndActiveTrue(Product product);
    Optional<ProductVariantExclusion> findByProductAndCombinationHash(Product product, String combinationHash);
}
