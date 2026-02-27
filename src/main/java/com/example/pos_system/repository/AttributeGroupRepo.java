package com.example.pos_system.repository;

import com.example.pos_system.entity.AttributeGroup;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface AttributeGroupRepo extends JpaRepository<AttributeGroup, Long> {
    /**
     * Executes the findByCodeIgnoreCase operation.
     *
     * @param code Parameter of type {@code String} used by this operation.
     * @return {@code Optional<AttributeGroup>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<AttributeGroup> findByCodeIgnoreCase(String code);
    /**
     * Executes the findAllByOrderBySortOrderAscNameAsc operation.
     *
     * @return {@code List<AttributeGroup>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<AttributeGroup> findAllByOrderBySortOrderAscNameAsc();
}
