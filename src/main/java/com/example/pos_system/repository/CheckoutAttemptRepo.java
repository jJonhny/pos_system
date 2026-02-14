package com.example.pos_system.repository;

import com.example.pos_system.entity.CheckoutAttempt;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import jakarta.persistence.LockModeType;
import java.util.Optional;

public interface CheckoutAttemptRepo extends JpaRepository<CheckoutAttempt, Long> {
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
            select c
            from CheckoutAttempt c
            where c.terminalId = :terminalId
              and c.clientCheckoutId = :clientCheckoutId
            """)
    Optional<CheckoutAttempt> findForUpdate(@Param("terminalId") String terminalId,
                                            @Param("clientCheckoutId") String clientCheckoutId);
}
