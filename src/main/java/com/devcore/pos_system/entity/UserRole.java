package com.devcore.pos_system.entity;

public enum UserRole {
    SUPER_ADMIN,
    BRANCH_MANAGER,
    INVENTORY_STAFF,
    ADMIN,
    MANAGER,
    CASHIER;

    /**
     * Executes the isSuperAdminLike operation.
     *
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public boolean isSuperAdminLike() {
        return this == SUPER_ADMIN || this == ADMIN;
    }

    /**
     * Executes the isBranchManagerLike operation.
     *
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public boolean isBranchManagerLike() {
        return this == BRANCH_MANAGER || this == MANAGER;
    }
}
