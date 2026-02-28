package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.UnitOfMeasure;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface UnitOfMeasureRepo extends JpaRepository<UnitOfMeasure, Long> {
    /**
     * Executes the findByCodeIgnoreCase operation.
     *
     * @param code Parameter of type {@code String} used by this operation.
     * @return {@code Optional<UnitOfMeasure>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    Optional<UnitOfMeasure> findByCodeIgnoreCase(String code);

    boolean existsByCodeIgnoreCase(String code);

    boolean existsByCodeIgnoreCaseAndIdNot(String code, Long id);

    List<UnitOfMeasure> findAllByOrderByActiveDescNameAscCodeAsc();

    List<UnitOfMeasure> findByActiveTrueOrderByNameAscCodeAsc();
}
