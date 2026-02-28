package com.devcore.pos_system.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.SQLRestriction;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@Entity
@SQLRestriction("deleted_status = 0")
@Table(indexes = {
        @Index(name = "idx_product_name", columnList = "name"),
        @Index(name = "idx_product_active", columnList = "active"),
        @Index(name = "idx_product_active_id", columnList = "active,id"),
        @Index(name = "idx_product_active_category_id", columnList = "active,category_id,id"),
        @Index(name = "idx_product_allow_negative_stock", columnList = "allow_negative_stock"),
        @Index(name = "idx_product_category", columnList = "category_id"),
        @Index(name = "idx_product_price", columnList = "price"),
        @Index(name = "idx_product_stock_qty", columnList = "stock_qty"),
        @Index(name = "idx_product_low_stock_threshold", columnList = "low_stock_threshold"),
        @Index(name = "idx_product_deleted_status", columnList = "deleted_status"),
        @Index(name = "idx_product_expiration_date", columnList = "expiration_date")
})
public class Product {
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(unique = true)
    private String sku;

    @Column(unique = true)
    private String barcode;

    private String name;
    private BigDecimal price;
    private BigDecimal wholesalePrice;
    private Integer wholesaleMinQty;
    private BigDecimal costPrice;
    private Integer stockQty;
    private Integer lowStockThreshold;
    private Integer unitsPerBox;
    private Integer unitsPerCase;
    @Column(length = 64)
    private String baseUnitName;
    @Column(nullable = false)
    private Integer baseUnitPrecision = 0;
    private Long retailPriceUnitId;
    private Long wholesalePriceUnitId;
    private Long wholesaleMinQtyUnitId;
    private Long lowStockThresholdUnitId;
    private Boolean active = true;
    @Column(nullable = false)
    private Boolean allowNegativeStock = false;
    @Column(length = 2048)
    private String imageUrl;
    @Column(length = 1024)
    private String boxSpecifications;
    @Column(precision = 12, scale = 3)
    private BigDecimal weightValue;
    @Column(length = 16)
    private String weightUnit;
    @Column(precision = 12, scale = 3)
    private BigDecimal lengthValue;
    @Column(length = 16)
    private String lengthUnit;
    @Column(precision = 12, scale = 3)
    private BigDecimal widthValue;
    @Column(length = 16)
    private String widthUnit;
    @Column(precision = 12, scale = 3)
    private BigDecimal heightValue;
    @Column(length = 16)
    private String heightUnit;
    @Column(length = 32)
    private String basicUnit;
    @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
    private LocalDate expirationDate;
    @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
    private LocalDate manufactureDate;
    @Column(nullable = false)
    private Boolean deletedStatus = false;
    @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
    private LocalDateTime updatedAt;
    @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
    private LocalDateTime deletedAt;

    @ManyToOne(fetch = FetchType.LAZY)
    private Category category;

    @OneToMany(mappedBy = "product", cascade = CascadeType.ALL, orphanRemoval = true)
    @OrderBy("conversionToBase ASC, id ASC")
    private List<ProductUnit> productUnits = new ArrayList<>();

    /**
     * Executes the isLowStock operation.
     *
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the isLowStock operation.
     *
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the isLowStock operation.
     *
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Transient
    public boolean isLowStock() {
        if (stockQty == null || lowStockThreshold == null) return false;
        return stockQty <= lowStockThreshold;
    }

    /**
     * Executes the onCreate operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the onCreate operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the onCreate operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PrePersist
    public void onCreate() {
        if (updatedAt == null) {
            updatedAt = LocalDateTime.now();
        }
        if (deletedStatus == null) {
            deletedStatus = false;
        }
        if (Boolean.TRUE.equals(deletedStatus) && deletedAt == null) {
            deletedAt = LocalDateTime.now();
        }
        if (baseUnitName == null || baseUnitName.trim().isEmpty()) {
            if (basicUnit != null && !basicUnit.trim().isEmpty()) {
                baseUnitName = basicUnit.trim();
            } else {
                baseUnitName = "piece";
            }
        }
        if (baseUnitPrecision == null || baseUnitPrecision < 0) {
            baseUnitPrecision = 0;
        }
    }

    /**
     * Executes the onUpdate operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the onUpdate operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the onUpdate operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @PreUpdate
    public void onUpdate() {
        updatedAt = LocalDateTime.now();
        if (Boolean.TRUE.equals(deletedStatus)) {
            if (deletedAt == null) {
                deletedAt = LocalDateTime.now();
            }
            if (active == null || active) {
                active = false;
            }
        } else {
            deletedAt = null;
        }
        if (baseUnitName == null || baseUnitName.trim().isEmpty()) {
            if (basicUnit != null && !basicUnit.trim().isEmpty()) {
                baseUnitName = basicUnit.trim();
            } else {
                baseUnitName = "piece";
            }
        } else {
            baseUnitName = baseUnitName.trim();
        }
        if (baseUnitPrecision == null || baseUnitPrecision < 0) {
            baseUnitPrecision = 0;
        }
    }
}
