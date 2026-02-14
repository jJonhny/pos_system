package com.example.pos_system.service;

import com.example.pos_system.entity.CheckoutAttempt;
import com.example.pos_system.entity.CheckoutAttemptStatus;
import com.example.pos_system.entity.Sale;
import com.example.pos_system.repository.CheckoutAttemptRepo;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

@Service
public class CheckoutAttemptService {
    private static final String UNKNOWN_TERMINAL = "UNKNOWN_TERMINAL";

    private final CheckoutAttemptRepo checkoutAttemptRepo;

    public CheckoutAttemptService(CheckoutAttemptRepo checkoutAttemptRepo) {
        this.checkoutAttemptRepo = checkoutAttemptRepo;
    }

    @Transactional
    public CheckoutResult process(String clientCheckoutId,
                                  String terminalId,
                                  CheckoutOperation operation) {
        String safeClientId = normalizeClientCheckoutId(clientCheckoutId);
        String safeTerminalId = normalizeTerminalId(terminalId);
        CheckoutAttempt attempt = reserveAttempt(safeClientId, safeTerminalId);

        if (attempt.getStatus() == CheckoutAttemptStatus.SUCCESS && attempt.getSale() != null) {
            return new CheckoutResult(attempt.getSale(), true);
        }

        try {
            Sale sale = operation.execute();
            if (sale == null) {
                throw new IllegalStateException("Checkout did not return a sale.");
            }
            attempt.setSale(sale);
            attempt.setStatus(CheckoutAttemptStatus.SUCCESS);
            attempt.setFailureReason(null);
            attempt.setCompletedAt(LocalDateTime.now());
            CheckoutAttempt saved = checkoutAttemptRepo.save(attempt);
            return new CheckoutResult(saved.getSale(), false);
        } catch (RuntimeException ex) {
            attempt.setStatus(CheckoutAttemptStatus.FAILED);
            attempt.setFailureReason(trim(ex.getMessage(), 512));
            attempt.setCompletedAt(LocalDateTime.now());
            checkoutAttemptRepo.save(attempt);
            throw ex;
        }
    }

    private CheckoutAttempt reserveAttempt(String clientCheckoutId, String terminalId) {
        var existing = checkoutAttemptRepo.findForUpdate(terminalId, clientCheckoutId);
        if (existing.isPresent()) {
            return existing.get();
        }
        CheckoutAttempt attempt = new CheckoutAttempt();
        attempt.setClientCheckoutId(clientCheckoutId);
        attempt.setTerminalId(terminalId);
        attempt.setStatus(CheckoutAttemptStatus.PENDING);
        try {
            return checkoutAttemptRepo.saveAndFlush(attempt);
        } catch (DataIntegrityViolationException ex) {
            return checkoutAttemptRepo.findForUpdate(terminalId, clientCheckoutId)
                    .orElseThrow(() -> ex);
        }
    }

    private String normalizeClientCheckoutId(String clientCheckoutId) {
        String cleaned = trim(clientCheckoutId, 64);
        if (cleaned == null) {
            throw new IllegalStateException("Missing client checkout id.");
        }
        return cleaned;
    }

    private String normalizeTerminalId(String terminalId) {
        String cleaned = trim(terminalId, 128);
        return cleaned == null ? UNKNOWN_TERMINAL : cleaned;
    }

    private String trim(String value, int maxLength) {
        if (value == null) return null;
        String cleaned = value.trim();
        if (cleaned.isEmpty()) return null;
        return cleaned.length() <= maxLength ? cleaned : cleaned.substring(0, maxLength);
    }

    public record CheckoutResult(Sale sale, boolean replayed) {}

    @FunctionalInterface
    public interface CheckoutOperation {
        Sale execute();
    }
}
