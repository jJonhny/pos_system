package com.devcore.pos_system.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Lob;
import jakarta.persistence.PrePersist;
import jakarta.persistence.Table;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.Immutable;

import java.time.LocalDateTime;

@Getter
@Entity
@Immutable
@Table(name = "audit_event")
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class AuditEvent {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(updatable = false, nullable = false)
    private Long id;

    @Column(nullable = false, updatable = false)
    private LocalDateTime timestamp;

    @Column(updatable = false)
    private Long actorUserId;

    @Column(length = 100, updatable = false)
    private String actorUsername;

    @Column(length = 80, nullable = false, updatable = false)
    private String actionType;

    @Column(length = 80, nullable = false, updatable = false)
    private String targetType;

    @Column(length = 128, updatable = false)
    private String targetId;

    @Lob
    @Column(columnDefinition = "TEXT", updatable = false)
    private String beforeJson;

    @Lob
    @Column(columnDefinition = "TEXT", updatable = false)
    private String afterJson;

    @Lob
    @Column(columnDefinition = "TEXT", updatable = false)
    private String metadataJson;

    @Column(length = 64, updatable = false)
    private String ipAddress;

    @Column(length = 128, updatable = false)
    private String terminalId;

    /**
     * Executes the AuditEvent operation.
     * <p>Return value: A fully initialized AuditEvent instance.</p>
     *
     * @param timestamp Parameter of type {@code LocalDateTime} used by this operation.
     * @param actorUserId Parameter of type {@code Long} used by this operation.
     * @param actorUsername Parameter of type {@code String} used by this operation.
     * @param actionType Parameter of type {@code String} used by this operation.
     * @param targetType Parameter of type {@code String} used by this operation.
     * @param targetId Parameter of type {@code String} used by this operation.
     * @param beforeJson Parameter of type {@code String} used by this operation.
     * @param afterJson Parameter of type {@code String} used by this operation.
     * @param metadataJson Parameter of type {@code String} used by this operation.
     * @param ipAddress Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private AuditEvent(LocalDateTime timestamp,
                       Long actorUserId,
                       String actorUsername,
                       String actionType,
                       String targetType,
                       String targetId,
                       String beforeJson,
                       String afterJson,
                       String metadataJson,
                       String ipAddress,
                       String terminalId) {
        this.timestamp = timestamp;
        this.actorUserId = actorUserId;
        this.actorUsername = actorUsername;
        this.actionType = actionType;
        this.targetType = targetType;
        this.targetId = targetId;
        this.beforeJson = beforeJson;
        this.afterJson = afterJson;
        this.metadataJson = metadataJson;
        this.ipAddress = ipAddress;
        this.terminalId = terminalId;
    }

    /**
     * Executes the of operation.
     *
     * @param timestamp Parameter of type {@code LocalDateTime} used by this operation.
     * @param actorUserId Parameter of type {@code Long} used by this operation.
     * @param actorUsername Parameter of type {@code String} used by this operation.
     * @param actionType Parameter of type {@code String} used by this operation.
     * @param targetType Parameter of type {@code String} used by this operation.
     * @param targetId Parameter of type {@code String} used by this operation.
     * @param beforeJson Parameter of type {@code String} used by this operation.
     * @param afterJson Parameter of type {@code String} used by this operation.
     * @param metadataJson Parameter of type {@code String} used by this operation.
     * @param ipAddress Parameter of type {@code String} used by this operation.
     * @param terminalId Parameter of type {@code String} used by this operation.
     * @return {@code AuditEvent} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public static AuditEvent of(LocalDateTime timestamp,
                                Long actorUserId,
                                String actorUsername,
                                String actionType,
                                String targetType,
                                String targetId,
                                String beforeJson,
                                String afterJson,
                                String metadataJson,
                                String ipAddress,
                                String terminalId) {
        return new AuditEvent(timestamp, actorUserId, actorUsername, actionType, targetType, targetId,
                beforeJson, afterJson, metadataJson, ipAddress, terminalId);
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
        if (timestamp == null) {
            timestamp = LocalDateTime.now();
        }
    }
}
