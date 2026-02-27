package com.example.pos_system.repository;

import com.example.pos_system.entity.HeldSale;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface HeldSaleRepo extends JpaRepository<HeldSale, Long> {
    /**
     * Executes the findByCashierUsernameOrderByCreatedAtDesc operation.
     *
     * @param cashierUsername Parameter of type {@code String} used by this operation.
     * @return {@code List<HeldSale>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<HeldSale> findByCashierUsernameOrderByCreatedAtDesc(String cashierUsername);
}
