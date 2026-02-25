package com.example.pos_system.entity;

public enum UserRole {
    SUPER_ADMIN,
    BRANCH_MANAGER,
    INVENTORY_STAFF,
    ADMIN,
    MANAGER,
    CASHIER;

    public boolean isSuperAdminLike() {
        return this == SUPER_ADMIN || this == ADMIN;
    }

    public boolean isBranchManagerLike() {
        return this == BRANCH_MANAGER || this == MANAGER;
    }
}
