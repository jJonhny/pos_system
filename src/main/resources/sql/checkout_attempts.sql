-- Idempotent checkout attempt tracking.
-- Apply manually in environments that do not rely on JPA schema auto-update.

CREATE TABLE IF NOT EXISTS checkout_attempt (
    id BIGINT NOT NULL AUTO_INCREMENT,
    terminal_id VARCHAR(128) NOT NULL,
    client_checkout_id VARCHAR(64) NOT NULL,
    status VARCHAR(24) NOT NULL,
    sale_id BIGINT NULL,
    failure_reason VARCHAR(512) NULL,
    created_at DATETIME(6) NOT NULL,
    updated_at DATETIME(6) NOT NULL,
    completed_at DATETIME(6) NULL,
    PRIMARY KEY (id),
    CONSTRAINT uk_checkout_attempt_terminal_client UNIQUE (terminal_id, client_checkout_id),
    CONSTRAINT fk_checkout_attempt_sale FOREIGN KEY (sale_id) REFERENCES sale (id)
);

CREATE INDEX idx_checkout_attempt_terminal_status ON checkout_attempt (terminal_id, status);
CREATE INDEX idx_checkout_attempt_created_at ON checkout_attempt (created_at);
