package com.example.pos_system.repository;

import com.example.pos_system.entity.Customer;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface CustomerRepo extends JpaRepository<Customer, Long> {
    /**
     * Executes the findByPhone operation.
     *
     * @param phone Parameter of type {@code String} used by this operation.
     * @return {@code Optional<Customer>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<Customer> findByPhone(String phone);
    /**
     * Executes the findByEmail operation.
     *
     * @param email Parameter of type {@code String} used by this operation.
     * @return {@code Optional<Customer>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<Customer> findByEmail(String email);
}
