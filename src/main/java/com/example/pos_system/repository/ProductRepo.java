package com.example.pos_system.repository;

import com.example.pos_system.entity.Product;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;

public interface ProductRepo extends JpaRepository<Product, Long>, JpaSpecificationExecutor<Product> {
    Page<Product> findByActiveTrue(Pageable pageable);
    Page<Product> findByActiveTrueAndNameContainingIgnoreCase(String q, Pageable pageable);
    Page<Product> findByActiveTrueAndCategory_Id(Long categoryId, Pageable pageable);
    Page<Product> findByCategory_Id(Long categoryId, Pageable pageable);
    Optional<Product> findByBarcode(String barcode);
    Optional<Product> findBySkuIgnoreCase(String sku);
    boolean existsByCategory_Id(Long categoryId);
    java.util.List<Product> findByCategory_Id(Long categoryId, Sort sort);

    @Query("select p.category.id as categoryId, count(p) as count from Product p where p.category is not null group by p.category.id")
    java.util.List<CategoryCount> countByCategory();

    @Query("""
            select p.category.id as categoryId, count(p) as count
            from Product p
            where p.category is not null
              and p.stockQty is not null
              and p.lowStockThreshold is not null
              and p.stockQty <= p.lowStockThreshold
            group by p.category.id
            """)
    java.util.List<CategoryCount> countLowStockByCategory();

    @Query("select count(p) from Product p where p.stockQty is not null and p.lowStockThreshold is not null and p.stockQty <= p.lowStockThreshold")
    long countLowStock();

    @Query("select p from Product p where p.stockQty is not null and p.lowStockThreshold is not null and p.stockQty <= p.lowStockThreshold")
    Page<Product> findLowStock(Pageable pageable);

    @Query("select p from Product p where p.stockQty is not null and p.lowStockThreshold is not null and p.stockQty <= p.lowStockThreshold and p.category.id = :categoryId")
    Page<Product> findLowStockByCategoryId(@Param("categoryId") Long categoryId, Pageable pageable);

    interface CategoryCount {
        Long getCategoryId();
        Long getCount();
    }
}
