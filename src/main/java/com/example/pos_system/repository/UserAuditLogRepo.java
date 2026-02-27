package com.example.pos_system.repository;

import com.example.pos_system.entity.UserAuditLog;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface UserAuditLogRepo extends JpaRepository<UserAuditLog, Long> {
    /**
     * Executes the findTop50ByOrderByCreatedAtDesc operation.
     *
     * @return {@code List<UserAuditLog>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<UserAuditLog> findTop50ByOrderByCreatedAtDesc();
}
