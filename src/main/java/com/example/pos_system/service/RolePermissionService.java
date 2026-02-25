package com.example.pos_system.service;

import com.example.pos_system.entity.Permission;
import com.example.pos_system.entity.UserRole;
import org.springframework.stereotype.Service;

import java.util.EnumSet;
import java.util.Set;

@Service
public class RolePermissionService {
    public Set<Permission> defaultsForRole(UserRole role) {
        if (role == null) {
            return EnumSet.noneOf(Permission.class);
        }

        if (role.isSuperAdminLike()) {
            return EnumSet.allOf(Permission.class);
        }

        if (role.isBranchManagerLike()) {
            return EnumSet.of(
                    Permission.VIEW_REPORTS,
                    Permission.VIEW_ANALYTICS,
                    Permission.MANAGE_INVENTORY,
                    Permission.MANAGE_SALES,
                    Permission.USE_POS,
                    Permission.POS_PRINT,
                    Permission.POS_DRAWER_OPEN,
                    Permission.POS_TERMINAL_SETTINGS,
                    Permission.INVENTORY_VIEW_MOVEMENTS,
                    Permission.PURCHASES_MANAGE,
                    Permission.RECEIVING_POST,
                    Permission.SUPPLIERS_MANAGE
            );
        }

        if (role == UserRole.INVENTORY_STAFF) {
            return EnumSet.of(
                    Permission.MANAGE_INVENTORY,
                    Permission.INVENTORY_VIEW_MOVEMENTS,
                    Permission.PURCHASES_MANAGE,
                    Permission.RECEIVING_POST,
                    Permission.SUPPLIERS_MANAGE
            );
        }

        return EnumSet.of(
                Permission.USE_POS,
                Permission.POS_PRINT,
                Permission.POS_DRAWER_OPEN
        );
    }
}
