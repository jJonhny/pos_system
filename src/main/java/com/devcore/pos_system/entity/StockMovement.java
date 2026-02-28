package com.devcore.pos_system.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.PrePersist;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Setter
@Entity
@Table(name = "stock_movement", indexes = {
        @Index(name = "idx_stock_movement_product_created", columnList = "product_id,created_at"),
        @Index(name = "idx_stock_movement_type_created", columnList = "movement_type,created_at")
})
public class StockMovement {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_id", nullable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    private Product product;

    @Column(nullable = false)
    private Integer qtyDelta;

    @Column(name = "unit_id")
    private Long unitId;

    @Column(name = "qty_base")
    private Integer qtyBase;

    @Column(precision = 18, scale = 4)
    private BigDecimal unitCost;

    @Column(length = 8)
    private String currency;

    @Enumerated(EnumType.STRING)
    @Column(name = "movement_type", nullable = false, length = 24)
    private StockMovementType type;

    @Column(length = 32)
    private String refType;

    @Column(length = 128)
    private String refId;

    @Column(nullable = false)
    private LocalDateTime createdAt;

    private Long actorUserId;

    @Column(length = 128)
    private String terminalId;

    @Column(length = 500)
    private String notes;

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
        if (createdAt == null) {
            createdAt = LocalDateTime.now();
        }
    }
}
