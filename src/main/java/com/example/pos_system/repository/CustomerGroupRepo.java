package com.example.pos_system.repository;

import com.example.pos_system.entity.CustomerGroup;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface CustomerGroupRepo extends JpaRepository<CustomerGroup, Long> {
    /**
     * Executes the findByCodeIgnoreCase operation.
     *
     * @param code Parameter of type {@code String} used by this operation.
     * @return {@code Optional<CustomerGroup>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<CustomerGroup> findByCodeIgnoreCase(String code);
}
