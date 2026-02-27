package com.example.pos_system.repository;

import com.example.pos_system.entity.AppUser;
import com.example.pos_system.entity.UserRole;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.Optional;

public interface AppUserRepo extends JpaRepository<AppUser, Long>, JpaSpecificationExecutor<AppUser> {
    /**
     * Executes the findByUsername operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @return {@code Optional<AppUser>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<AppUser> findByUsername(String username);
    /**
     * Executes the findByUsernameIgnoreCase operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @return {@code Optional<AppUser>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<AppUser> findByUsernameIgnoreCase(String username);
    /**
     * Executes the findByEmailIgnoreCase operation.
     *
     * @param email Parameter of type {@code String} used by this operation.
     * @return {@code Optional<AppUser>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<AppUser> findByEmailIgnoreCase(String email);
    /**
     * Executes the findByUsernameIgnoreCaseOrEmailIgnoreCase operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @param email Parameter of type {@code String} used by this operation.
     * @return {@code Optional<AppUser>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<AppUser> findByUsernameIgnoreCaseOrEmailIgnoreCase(String username, String email);
    /**
     * Executes the existsByUsernameIgnoreCase operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    boolean existsByUsernameIgnoreCase(String username);
    /**
     * Executes the existsByEmailIgnoreCase operation.
     *
     * @param email Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    boolean existsByEmailIgnoreCase(String email);
    /**
     * Executes the countByRole operation.
     *
     * @param role Parameter of type {@code UserRole} used by this operation.
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    long countByRole(UserRole role);
}
