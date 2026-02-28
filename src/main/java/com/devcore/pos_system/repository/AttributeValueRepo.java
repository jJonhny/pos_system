package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.AttributeGroup;
import com.devcore.pos_system.entity.AttributeValue;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

public interface AttributeValueRepo extends JpaRepository<AttributeValue, Long> {
    /**
     * Executes the findByGroupAndCodeIgnoreCase operation.
     *
     * @param group Parameter of type {@code AttributeGroup} used by this operation.
     * @param code Parameter of type {@code String} used by this operation.
     * @return {@code Optional<AttributeValue>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<AttributeValue> findByGroupAndCodeIgnoreCase(AttributeGroup group, String code);
    /**
     * Executes the findByGroupOrderBySortOrderAscDisplayNameAsc operation.
     *
     * @param group Parameter of type {@code AttributeGroup} used by this operation.
     * @return {@code List<AttributeValue>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<AttributeValue> findByGroupOrderBySortOrderAscDisplayNameAsc(AttributeGroup group);
    /**
     * Executes the findByIdIn operation.
     *
     * @param ids Parameter of type {@code Collection<Long>} used by this operation.
     * @return {@code List<AttributeValue>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<AttributeValue> findByIdIn(Collection<Long> ids);
}
