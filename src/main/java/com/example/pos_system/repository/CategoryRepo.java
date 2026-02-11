package com.example.pos_system.repository;

import com.example.pos_system.entity.Category;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;

public interface CategoryRepo extends JpaRepository<Category, Long> {
    Optional<Category> findByNameIgnoreCase(String name);

    @Query("""
            select c from Category c
            where (:q is null or :q = ''
              or lower(c.name) like lower(concat('%', :q, '%'))
              or lower(c.description) like lower(concat('%', :q, '%')))
              and (:active is null or coalesce(c.active, false) = :active)
            """)
    Page<Category> search(@Param("q") String q,
                          @Param("active") Boolean active,
                          Pageable pageable);

    @Query("select coalesce(max(c.sortOrder), 0) from Category c")
    Integer findMaxSortOrder();

    @Query("select count(c) from Category c where c.active = true")
    long countByActiveTrue();

    @Query("select count(c) from Category c where c.active = false or c.active is null")
    long countByActiveFalse();
}
