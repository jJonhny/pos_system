-- Immutable audit log table for sensitive POS actions.
-- Apply manually in environments that do not rely on JPA schema auto-update.

CREATE TABLE IF NOT EXISTS audit_event (
    id BIGINT NOT NULL AUTO_INCREMENT,
    timestamp DATETIME(6) NOT NULL,
    actor_user_id BIGINT NULL,
    actor_username VARCHAR(100) NULL,
    action_type VARCHAR(80) NOT NULL,
    target_type VARCHAR(80) NOT NULL,
    target_id VARCHAR(128) NULL,
    before_json TEXT NULL,
    after_json TEXT NULL,
    metadata_json TEXT NULL,
    ip_address VARCHAR(64) NULL,
    terminal_id VARCHAR(128) NULL,
    PRIMARY KEY (id)
);

CREATE INDEX idx_audit_event_timestamp ON audit_event (timestamp);
CREATE INDEX idx_audit_event_actor_username ON audit_event (actor_username);
CREATE INDEX idx_audit_event_action_type ON audit_event (action_type);
