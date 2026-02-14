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

    public SupplierService(SupplierRepo supplierRepo, AuditEventService auditEventService) {
        this.supplierRepo = supplierRepo;
        this.auditEventService = auditEventService;
    }

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

    @Transactional(readOnly = true)
    public Supplier get(Long id) {
        requireManageSuppliers();
        if (id == null) return null;
        return supplierRepo.findById(id).orElse(null);
    }

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

    public void delete(Long id) {
        requireManageSuppliers();
        Supplier supplier = supplierRepo.findById(id).orElseThrow(() -> new IllegalArgumentException("Supplier not found."));
        var before = snapshot(supplier);
        supplierRepo.delete(supplier);
        auditEventService.record("SUPPLIER_DELETE", "SUPPLIER", id, before, null, null);
    }

    private void requireManageSuppliers() {
        if (!hasAnyAuthority("PERM_SUPPLIERS_MANAGE", "ROLE_ADMIN", "ROLE_MANAGER")) {
            throw new AccessDeniedException("Supplier management permission required.");
        }
    }

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

    private String trimTo(String value, int maxLength) {
        if (value == null) return null;
        String trimmed = value.trim();
        if (trimmed.isEmpty()) return null;
        return trimmed.length() <= maxLength ? trimmed : trimmed.substring(0, maxLength);
    }

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
