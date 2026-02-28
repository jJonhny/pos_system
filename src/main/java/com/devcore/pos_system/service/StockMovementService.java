package com.devcore.pos_system.service;

import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.StockMovement;
import com.devcore.pos_system.entity.StockMovementType;
import com.devcore.pos_system.repository.AppUserRepo;
import com.devcore.pos_system.repository.ProductRepo;
import com.devcore.pos_system.repository.StockMovementRepo;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.Locale;

@Service
@Transactional
public class StockMovementService {
    private final ProductRepo productRepo;
    private final StockMovementRepo stockMovementRepo;
    private final AppUserRepo appUserRepo;

    /**
     * Executes the StockMovementService operation.
     * <p>Return value: A fully initialized StockMovementService instance.</p>
     *
     * @param productRepo Parameter of type {@code ProductRepo} used by this operation.
     * @param stockMovementRepo Parameter of type {@code StockMovementRepo} used by this operation.
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public StockMovementService(ProductRepo productRepo,
                                StockMovementRepo stockMovementRepo,
                                AppUserRepo appUserRepo) {
        this.productRepo = productRepo;
        this.stockMovementRepo = stockMovementRepo;
        this.appUserRepo = appUserRepo;
    }

    /**
     * Executes the recordSale operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param soldQty Parameter of type {@code int} used by this operation.
     * @param unitCost Parameter of type {@code BigDecimal} used by this operation.
     * @param currency Parameter of type {@code String} used by this operation.
     * @param refType Parameter of type {@code String} used by this operation.
     * @param refId Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Product recordSale(Long productId,
                              int soldQty,
                              BigDecimal unitCost,
                              String currency,
                              String refType,
                              String refId,
                              String terminalId,
                              String notes) {
        return recordSale(productId, soldQty, null, unitCost, currency, refType, refId, terminalId, notes);
    }

    public Product recordSale(Long productId,
                              int soldQty,
                              Long unitId,
                              BigDecimal unitCost,
                              String currency,
                              String refType,
                              String refId,
                              String terminalId,
                              String notes) {
        if (soldQty <= 0) {
            throw new IllegalArgumentException("Sold quantity must be greater than zero.");
        }
        return applyDelta(productId, -soldQty, unitId, -soldQty, unitCost, currency,
                StockMovementType.SALE, refType, refId, terminalId, notes);
    }

    /**
     * Executes the recordReturn operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param qty Parameter of type {@code int} used by this operation.
     * @param unitCost Parameter of type {@code BigDecimal} used by this operation.
     * @param currency Parameter of type {@code String} used by this operation.
     * @param refType Parameter of type {@code String} used by this operation.
     * @param refId Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Product recordReturn(Long productId,
                                int qty,
                                BigDecimal unitCost,
                                String currency,
                                String refType,
                                String refId,
                                String terminalId,
                                String notes) {
        return recordReturn(productId, qty, null, unitCost, currency, refType, refId, terminalId, notes);
    }

    public Product recordReturn(Long productId,
                                int qty,
                                Long unitId,
                                BigDecimal unitCost,
                                String currency,
                                String refType,
                                String refId,
                                String terminalId,
                                String notes) {
        if (qty <= 0) {
            throw new IllegalArgumentException("Return quantity must be greater than zero.");
        }
        return applyDelta(productId, qty, unitId, qty, unitCost, currency,
                StockMovementType.RETURN, refType, refId, terminalId, notes);
    }

    /**
     * Executes the recordVoid operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param qty Parameter of type {@code int} used by this operation.
     * @param unitCost Parameter of type {@code BigDecimal} used by this operation.
     * @param currency Parameter of type {@code String} used by this operation.
     * @param refType Parameter of type {@code String} used by this operation.
     * @param refId Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Product recordVoid(Long productId,
                              int qty,
                              BigDecimal unitCost,
                              String currency,
                              String refType,
                              String refId,
                              String terminalId,
                              String notes) {
        return recordVoid(productId, qty, null, unitCost, currency, refType, refId, terminalId, notes);
    }

    public Product recordVoid(Long productId,
                              int qty,
                              Long unitId,
                              BigDecimal unitCost,
                              String currency,
                              String refType,
                              String refId,
                              String terminalId,
                              String notes) {
        if (qty <= 0) {
            throw new IllegalArgumentException("Void quantity must be greater than zero.");
        }
        return applyDelta(productId, qty, unitId, qty, unitCost, currency,
                StockMovementType.VOID, refType, refId, terminalId, notes);
    }

    /**
     * Executes the recordReceive operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param qty Parameter of type {@code int} used by this operation.
     * @param unitCost Parameter of type {@code BigDecimal} used by this operation.
     * @param currency Parameter of type {@code String} used by this operation.
     * @param refType Parameter of type {@code String} used by this operation.
     * @param refId Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Product recordReceive(Long productId,
                                 int qty,
                                 BigDecimal unitCost,
                                 String currency,
                                 String refType,
                                 String refId,
                                 String terminalId,
                                 String notes) {
        return recordReceive(productId, qty, null, unitCost, currency, refType, refId, terminalId, notes);
    }

    public Product recordReceive(Long productId,
                                 int qty,
                                 Long unitId,
                                 BigDecimal unitCost,
                                 String currency,
                                 String refType,
                                 String refId,
                                 String terminalId,
                                 String notes) {
        if (qty <= 0) {
            throw new IllegalArgumentException("Received quantity must be greater than zero.");
        }
        return applyDelta(productId, qty, unitId, qty, unitCost, currency,
                StockMovementType.RECEIVE, refType, refId, terminalId, notes);
    }

    /**
     * Executes the adjustToTarget operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param targetOnHand Parameter of type {@code int} used by this operation.
     * @param unitCost Parameter of type {@code BigDecimal} used by this operation.
     * @param currency Parameter of type {@code String} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @param refType Parameter of type {@code String} used by this operation.
     * @param refId Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Product adjustToTarget(Long productId,
                                  int targetOnHand,
                                  BigDecimal unitCost,
                                  String currency,
                                  StockMovementType type,
                                  String refType,
                                  String refId,
                                  String terminalId,
                                  String notes) {
        return adjustToTarget(productId, targetOnHand, null, unitCost, currency, type, refType, refId, terminalId, notes);
    }

    public Product adjustToTarget(Long productId,
                                  int targetOnHand,
                                  Long unitId,
                                  BigDecimal unitCost,
                                  String currency,
                                  StockMovementType type,
                                  String refType,
                                  String refId,
                                  String terminalId,
                                  String notes) {
        Product locked = lockProduct(productId);
        if (targetOnHand < 0 && !Boolean.TRUE.equals(locked.getAllowNegativeStock())) {
            throw new IllegalStateException("Target stock cannot be negative for this product.");
        }
        int current = safeStock(locked.getStockQty());
        int delta = targetOnHand - current;
        if (delta == 0) {
            return locked;
        }
        return applyLocked(locked, delta, unitId, delta, unitCost, currency, type, refType, refId, terminalId, notes);
    }

    /**
     * Executes the adjustByDelta operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param qtyDelta Parameter of type {@code int} used by this operation.
     * @param unitCost Parameter of type {@code BigDecimal} used by this operation.
     * @param currency Parameter of type {@code String} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @param refType Parameter of type {@code String} used by this operation.
     * @param refId Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Product adjustByDelta(Long productId,
                                 int qtyDelta,
                                 BigDecimal unitCost,
                                 String currency,
                                 StockMovementType type,
                                 String refType,
                                 String refId,
                                 String terminalId,
                                  String notes) {
        return adjustByDelta(productId, qtyDelta, null, unitCost, currency, type, refType, refId, terminalId, notes);
    }

    public Product adjustByDelta(Long productId,
                                 int qtyDelta,
                                 Long unitId,
                                 BigDecimal unitCost,
                                 String currency,
                                 StockMovementType type,
                                 String refType,
                                 String refId,
                                 String terminalId,
                                 String notes) {
        if (qtyDelta == 0) {
            throw new IllegalArgumentException("Stock change cannot be zero.");
        }
        return applyDelta(productId, qtyDelta, unitId, qtyDelta, unitCost, currency, type, refType, refId, terminalId, notes);
    }

    /**
     * Executes the findMovements operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @return {@code List<StockMovement>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findMovements operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @return {@code List<StockMovement>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the findMovements operation.
     *
     * @param from Parameter of type {@code LocalDate} used by this operation.
     * @param to Parameter of type {@code LocalDate} used by this operation.
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @return {@code List<StockMovement>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public List<StockMovement> findMovements(LocalDate from,
                                             LocalDate to,
                                             Long productId,
                                             StockMovementType type) {
        requireMovementView();
        Specification<StockMovement> spec = (root, query, cb) -> cb.conjunction();
        LocalDateTime start = from == null ? null : from.atStartOfDay();
        LocalDateTime end = to == null ? null : to.atTime(LocalTime.MAX);

        if (start != null) {
            spec = spec.and((root, query, cb) -> cb.greaterThanOrEqualTo(root.get("createdAt"), start));
        }
        if (end != null) {
            spec = spec.and((root, query, cb) -> cb.lessThanOrEqualTo(root.get("createdAt"), end));
        }
        if (productId != null) {
            spec = spec.and((root, query, cb) -> cb.equal(root.get("product").get("id"), productId));
        }
        if (type != null) {
            spec = spec.and((root, query, cb) -> cb.equal(root.get("type"), type));
        }

        return stockMovementRepo.findAll(spec, Sort.by(Sort.Direction.DESC, "createdAt", "id"));
    }

    /**
     * Executes the applyDelta operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param qtyDelta Parameter of type {@code int} used by this operation.
     * @param unitCost Parameter of type {@code BigDecimal} used by this operation.
     * @param currency Parameter of type {@code String} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @param refType Parameter of type {@code String} used by this operation.
     * @param refId Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Product applyDelta(Long productId,
                               int qtyDelta,
                               Long unitId,
                               Integer qtyBase,
                               BigDecimal unitCost,
                               String currency,
                               StockMovementType type,
                               String refType,
                               String refId,
                               String terminalId,
                               String notes) {
        Product locked = lockProduct(productId);
        return applyLocked(locked, qtyDelta, unitId, qtyBase, unitCost, currency, type, refType, refId, terminalId, notes);
    }

    /**
     * Executes the applyLocked operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @param qtyDelta Parameter of type {@code int} used by this operation.
     * @param unitCost Parameter of type {@code BigDecimal} used by this operation.
     * @param currency Parameter of type {@code String} used by this operation.
     * @param type Parameter of type {@code StockMovementType} used by this operation.
     * @param refType Parameter of type {@code String} used by this operation.
     * @param refId Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @param notes Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Product applyLocked(Product product,
                                int qtyDelta,
                                Long unitId,
                                Integer qtyBase,
                                BigDecimal unitCost,
                                String currency,
                                StockMovementType type,
                                String refType,
                                String refId,
                                String terminalId,
                                String notes) {
        if (product == null || product.getId() == null) {
            throw new IllegalArgumentException("Product not found.");
        }
        if (qtyDelta == 0) {
            throw new IllegalArgumentException("Stock change cannot be zero.");
        }
        if (type == null) {
            throw new IllegalArgumentException("Stock movement type is required.");
        }
        if (type == StockMovementType.SALE && qtyDelta >= 0) {
            throw new IllegalArgumentException("SALE movement must be negative.");
        }

        int current = safeStock(product.getStockQty());
        int next = current + qtyDelta;
        if (next < 0 && !Boolean.TRUE.equals(product.getAllowNegativeStock())) {
            throw new IllegalStateException("Insufficient stock for " + safeName(product) + ".");
        }

        product.setStockQty(next);
        Product savedProduct = productRepo.save(product);

        StockMovement movement = new StockMovement();
        movement.setProduct(savedProduct);
        movement.setQtyDelta(qtyDelta);
        movement.setUnitId(unitId);
        movement.setQtyBase(qtyBase == null ? qtyDelta : qtyBase);
        movement.setUnitCost(scaleMoney(unitCost));
        movement.setCurrency(normalizeCode(currency, 8));
        movement.setType(type);
        movement.setRefType(normalizeCode(refType, 32));
        movement.setRefId(trimTo(refId, 128));
        movement.setTerminalId(trimTo(terminalId, 128));
        movement.setNotes(trimTo(notes, 500));
        movement.setActorUserId(resolveActorUserId());
        stockMovementRepo.save(movement);

        return savedProduct;
    }

    /**
     * Executes the lockProduct operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Product lockProduct(Long productId) {
        if (productId == null) {
            throw new IllegalArgumentException("Product not found.");
        }
        return productRepo.findByIdForUpdate(productId)
                .orElseThrow(() -> new IllegalArgumentException("Product not found."));
    }

    /**
     * Executes the safeStock operation.
     *
     * @param stockQty Parameter of type {@code Integer} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int safeStock(Integer stockQty) {
        return stockQty == null ? 0 : stockQty;
    }

    /**
     * Executes the scaleMoney operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal scaleMoney(BigDecimal value) {
        if (value == null) return null;
        return value.setScale(4, RoundingMode.HALF_UP);
    }

    /**
     * Executes the normalizeCode operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @param maxLength Parameter of type {@code int} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalizeCode(String value, int maxLength) {
        String trimmed = trimTo(value, maxLength);
        return trimmed == null ? null : trimmed.toUpperCase(Locale.ROOT);
    }

    /**
     * Executes the trimTo operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @param maxLength Parameter of type {@code int} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String trimTo(String value, int maxLength) {
        if (value == null) return null;
        String trimmed = value.trim();
        if (trimmed.isEmpty()) return null;
        return trimmed.length() <= maxLength ? trimmed : trimmed.substring(0, maxLength);
    }

    /**
     * Executes the safeName operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String safeName(Product product) {
        if (product == null || product.getName() == null || product.getName().isBlank()) {
            return "product #" + (product == null ? "?" : product.getId());
        }
        return product.getName();
    }

    /**
     * Executes the resolveActorUserId operation.
     *
     * @return {@code Long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Long resolveActorUserId() {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth == null || !auth.isAuthenticated()) {
            return null;
        }
        String username = auth.getName();
        if (username == null || username.isBlank() || "anonymousUser".equalsIgnoreCase(username)) {
            return null;
        }
        return appUserRepo.findByUsername(username).map(u -> u.getId()).orElse(null);
    }

    /**
     * Executes the requireMovementView operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void requireMovementView() {
        if (hasAuthority("ROLE_ADMIN")
                || hasAuthority("ROLE_MANAGER")
                || hasAuthority("PERM_INVENTORY_VIEW_MOVEMENTS")
                || hasAuthority("PERM_MANAGE_INVENTORY")
                || hasAuthority("PERM_VIEW_REPORTS")) {
            return;
        }
        throw new AccessDeniedException("Inventory movement view permission required.");
    }

    /**
     * Executes the hasAuthority operation.
     *
     * @param expected Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean hasAuthority(String expected) {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth == null || !auth.isAuthenticated()) return false;
        return auth.getAuthorities().stream().anyMatch(a -> expected.equalsIgnoreCase(a.getAuthority()));
    }
}
