package com.devcore.pos_system.api.v1.mapper;

import com.devcore.pos_system.api.v1.dto.inventory.StockAvailabilityDto;
import com.devcore.pos_system.api.v1.dto.inventory.StockMovementDto;
import com.devcore.pos_system.api.v1.dto.product.ApiProductDto;
import com.devcore.pos_system.api.v1.dto.product.ApiProductUnitDto;
import com.devcore.pos_system.api.v1.dto.supplier.ApiSupplierDto;
import com.devcore.pos_system.api.v1.dto.user.ApiUserDto;
import com.devcore.pos_system.entity.AppUser;
import com.devcore.pos_system.entity.Permission;
import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.ProductUnit;
import com.devcore.pos_system.entity.StockMovement;
import com.devcore.pos_system.entity.Supplier;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Set;

@Component
public class ApiV1Mapper {
    public ApiUserDto toUserDto(AppUser user) {
        if (user == null) {
            return null;
        }
        return new ApiUserDto(
                user.getId(),
                user.getUsername(),
                user.getEmail(),
                user.getRole() == null ? null : user.getRole().name(),
                Boolean.TRUE.equals(user.getActive()),
                Boolean.TRUE.equals(user.getMustResetPassword()),
                Boolean.TRUE.equals(user.getMfaRequired()),
                toPermissionNames(user.getPermissions()),
                user.getLastLoginAt()
        );
    }

    public ApiProductDto toProductDto(Product product) {
        if (product == null) {
            return null;
        }
        return new ApiProductDto(
                product.getId(),
                product.getSku(),
                product.getBarcode(),
                product.getName(),
                product.getPrice(),
                product.getCostPrice(),
                product.getStockQty(),
                product.getLowStockThreshold(),
                product.isLowStock(),
                Boolean.TRUE.equals(product.getActive()),
                Boolean.TRUE.equals(product.getAllowNegativeStock()),
                product.getBaseUnitName(),
                product.getBaseUnitPrecision(),
                product.getRetailPriceUnitId(),
                product.getWholesalePriceUnitId(),
                product.getWholesaleMinQtyUnitId(),
                product.getLowStockThresholdUnitId(),
                product.getUnitsPerBox(),
                product.getUnitsPerCase(),
                product.getCategory() == null ? null : product.getCategory().getId(),
                product.getCategory() == null ? null : product.getCategory().getName(),
                product.getImageUrl(),
                product.getUpdatedAt(),
                toProductUnitDtos(product.getProductUnits())
        );
    }

    public ApiProductUnitDto toProductUnitDto(ProductUnit unit) {
        if (unit == null) {
            return null;
        }
        return new ApiProductUnitDto(
                unit.getId(),
                unit.getName(),
                unit.getAbbreviation(),
                unit.getConversionToBase(),
                Boolean.TRUE.equals(unit.getAllowForSale()),
                Boolean.TRUE.equals(unit.getAllowForPurchase()),
                Boolean.TRUE.equals(unit.getIsDefaultSaleUnit()),
                Boolean.TRUE.equals(unit.getIsDefaultPurchaseUnit()),
                unit.getBarcode()
        );
    }

    public StockMovementDto toStockMovementDto(StockMovement movement) {
        if (movement == null) {
            return null;
        }
        Product product = movement.getProduct();
        return new StockMovementDto(
                movement.getId(),
                product == null ? null : product.getId(),
                product == null ? null : product.getName(),
                product == null ? null : product.getSku(),
                movement.getQtyDelta(),
                movement.getUnitCost(),
                movement.getCurrency(),
                movement.getType() == null ? null : movement.getType().name(),
                movement.getRefType(),
                movement.getRefId(),
                movement.getCreatedAt(),
                movement.getActorUserId(),
                movement.getTerminalId(),
                movement.getNotes()
        );
    }

    public StockAvailabilityDto toAvailabilityDto(Product product) {
        if (product == null) {
            return null;
        }
        return new StockAvailabilityDto(
                product.getId(),
                product.getName(),
                product.getSku(),
                product.getStockQty(),
                product.getLowStockThreshold(),
                product.isLowStock(),
                Boolean.TRUE.equals(product.getActive()),
                Boolean.TRUE.equals(product.getAllowNegativeStock())
        );
    }

    public ApiSupplierDto toSupplierDto(Supplier supplier) {
        if (supplier == null) {
            return null;
        }
        return new ApiSupplierDto(
                supplier.getId(),
                supplier.getName(),
                supplier.getPhone(),
                supplier.getEmail(),
                supplier.getAddress(),
                supplier.getStatus() == null ? null : supplier.getStatus().name()
        );
    }

    private List<String> toPermissionNames(Set<Permission> permissions) {
        if (permissions == null || permissions.isEmpty()) {
            return List.of();
        }
        return permissions.stream().map(Enum::name).sorted().toList();
    }

    private List<ApiProductUnitDto> toProductUnitDtos(List<ProductUnit> units) {
        if (units == null || units.isEmpty()) {
            return List.of();
        }
        return units.stream().map(this::toProductUnitDto).toList();
    }
}
