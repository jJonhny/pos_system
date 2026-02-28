package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.Category;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;

public interface CategoryRepo extends JpaRepository<Category, Long> {
    /**
     * Executes the findByNameIgnoreCase operation.
     *
     * @param name Parameter of type {@code String} used by this operation.
     * @return {@code Optional<Category>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<Category> findByNameIgnoreCase(String name);

    @Query("""
            select c from Category c
            where (:q is null or :q = ''
              /**
               * Executes the lower operation.
               *
               * @param name Parameter of type {@code c.} used by this operation.
               * @return {@code or} Result produced by this operation.
               * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
               * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
               */
              or lower(c.name) like lower(concat('%', :q, '%'))
              /**
               * Executes the lower operation.
               *
               * @param description Parameter of type {@code c.} used by this operation.
               * @return {@code or} Result produced by this operation.
               * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
               * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
               */
              or lower(c.description) like lower(concat('%', :q, '%')))
              and (:active is null or coalesce(c.active, false) = :active)
            """)
    /**
     * Executes the search operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param active Parameter of type {@code Boolean} used by this operation.
     * @param pageable Parameter of type {@code Pageable} used by this operation.
     * @return {@code Page<Category>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Page<Category> search(@Param("q") String q,
                          @Param("active") Boolean active,
                          Pageable pageable);

    /**
     * Executes the findMaxSortOrder operation.
     *
     * @return {@code Integer} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findMaxSortOrder operation.
     *
     * @return {@code Integer} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findMaxSortOrder operation.
     *
     * @return {@code Integer} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Query("select coalesce(max(c.sortOrder), 0) from Category c")
    Integer findMaxSortOrder();

    /**
     * Executes the countByActiveTrue operation.
     *
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the countByActiveTrue operation.
     *
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the countByActiveTrue operation.
     *
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Query("select count(c) from Category c where c.active = true")
    long countByActiveTrue();

    /**
     * Executes the countByActiveFalse operation.
     *
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the countByActiveFalse operation.
     *
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the countByActiveFalse operation.
     *
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Query("select count(c) from Category c where c.active = false or c.active is null")
    long countByActiveFalse();
}
