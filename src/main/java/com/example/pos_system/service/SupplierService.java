package com.example.pos_system.service;

import com.example.pos_system.entity.Supplier;
import com.example.pos_system.entity.SupplierStatus;
import com.example.pos_system.repository.SupplierRepo;
import org.springframework.data.domain.Sort;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Locale;

@Service
@Transactional
public class SupplierService {
    private final SupplierRepo supplierRepo;
    private final AuditEventService auditEventService;

    /**
     * Executes the SupplierService operation.
     * <p>Return value: A fully initialized SupplierService instance.</p>
     *
     * @param supplierRepo Parameter of type {@code SupplierRepo} used by this operation.
     * @param auditEventService Parameter of type {@code AuditEventService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public SupplierService(SupplierRepo supplierRepo, AuditEventService auditEventService) {
        this.supplierRepo = supplierRepo;
        this.auditEventService = auditEventService;
    }

    /**
     * Executes the list operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code SupplierStatus} used by this operation.
     * @return {@code List<Supplier>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the list operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code SupplierStatus} used by this operation.
     * @return {@code List<Supplier>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the list operation.
     *
     * @param q Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code SupplierStatus} used by this operation.
     * @return {@code List<Supplier>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public List<Supplier> list(String q, SupplierStatus status) {
        requireManageSuppliers();
        Sort sort = Sort.by("name").ascending();
        if (q != null && !q.isBlank() && status != null) {
            return supplierRepo.findByNameContainingIgnoreCaseAndStatus(q.trim(), status, sort);
        }
        if (q != null && !q.isBlank()) {
            return supplierRepo.findByNameContainingIgnoreCase(q.trim(), sort);
        }
        if (status != null) {
            return supplierRepo.findByStatus(status, sort);
        }
        return supplierRepo.findAll(sort);
    }

    /**
     * Executes the get operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Supplier} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the get operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Supplier} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the get operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return {@code Supplier} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transactional(readOnly = true)
    public Supplier get(Long id) {
        requireManageSuppliers();
        if (id == null) return null;
        return supplierRepo.findById(id).orElse(null);
    }

    /**
     * Executes the save operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param phone Parameter of type {@code String} used by this operation.
     * @param email Parameter of type {@code String} used by this operation.
     * @param address Parameter of type {@code String} used by this operation.
     * @param status Parameter of type {@code SupplierStatus} used by this operation.
     * @return {@code Supplier} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Supplier save(Long id,
                         String name,
                         String phone,
                         String email,
                         String address,
                         SupplierStatus status) {
        requireManageSuppliers();
        String supplierName = trimTo(name, 180);
        if (supplierName == null) {
            throw new IllegalArgumentException("Supplier name is required.");
        }

        Supplier supplier = id == null
                /**
                 * Executes the Supplier operation.
                 *
                 * @return {@code ? new} Result produced by this operation.
                 * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
                 * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
                 */
                ? new Supplier()
                : supplierRepo.findById(id).orElseThrow(() -> new IllegalArgumentException("Supplier not found."));

        var before = id == null ? null : snapshot(supplier);
        supplier.setName(supplierName);
        supplier.setPhone(trimTo(phone, 60));
        supplier.setEmail(trimTo(email, 180));
        supplier.setAddress(trimTo(address, 500));
        supplier.setStatus(status == null ? SupplierStatus.ACTIVE : status);

        Supplier saved = supplierRepo.save(supplier);
        String action = id == null ? "SUPPLIER_CREATE" : "SUPPLIER_UPDATE";
        auditEventService.record(action, "SUPPLIER", saved.getId(), before, snapshot(saved), null);
        return saved;
    }

    /**
     * Executes the delete operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void delete(Long id) {
        requireManageSuppliers();
        Supplier supplier = supplierRepo.findById(id).orElseThrow(() -> new IllegalArgumentException("Supplier not found."));
        var before = snapshot(supplier);
        supplierRepo.delete(supplier);
        auditEventService.record("SUPPLIER_DELETE", "SUPPLIER", id, before, null, null);
    }

    /**
     * Executes the requireManageSuppliers operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void requireManageSuppliers() {
        if (!hasAnyAuthority("PERM_SUPPLIERS_MANAGE", "ROLE_ADMIN", "ROLE_MANAGER")) {
            throw new AccessDeniedException("Supplier management permission required.");
        }
    }

    /**
     * Executes the hasAnyAuthority operation.
     *
     * @param authorities Parameter of type {@code String...} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean hasAnyAuthority(String... authorities) {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth == null || !auth.isAuthenticated()) {
            return false;
        }
        for (var granted : auth.getAuthorities()) {
            String value = granted.getAuthority();
            if (value == null) continue;
            for (String authority : authorities) {
                if (authority.equalsIgnoreCase(value)) {
                    return true;
                }
            }
        }
        return false;
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
     * Executes the snapshot operation.
     *
     * @param supplier Parameter of type {@code Supplier} used by this operation.
     * @return {@code java.util.Map<String, Object>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private java.util.Map<String, Object> snapshot(Supplier supplier) {
        if (supplier == null) return null;
        var map = new java.util.LinkedHashMap<String, Object>();
        map.put("id", supplier.getId());
        map.put("name", supplier.getName());
        map.put("phone", supplier.getPhone());
        map.put("email", supplier.getEmail());
        map.put("address", supplier.getAddress());
        map.put("status", supplier.getStatus() == null ? null : supplier.getStatus().name().toUpperCase(Locale.ROOT));
        return map;
    }
}
