-- POS hardware terminal/register settings schema updates.
-- Apply manually when not using JPA auto-update.

CREATE TABLE IF NOT EXISTS terminal_settings (
    id BIGINT NOT NULL AUTO_INCREMENT,
    terminal_id VARCHAR(128) NOT NULL,
    name VARCHAR(120) NOT NULL,
    default_currency VARCHAR(8) NULL,
    receipt_header VARCHAR(255) NULL,
    receipt_footer VARCHAR(500) NULL,
    tax_id VARCHAR(64) NULL,
    printer_mode VARCHAR(16) NOT NULL,
    bridge_url VARCHAR(255) NULL,
    auto_print_enabled BIT(1) NOT NULL DEFAULT b'0',
    camera_scanner_enabled BIT(1) NOT NULL DEFAULT b'0',
    created_at DATETIME(6) NOT NULL,
    updated_at DATETIME(6) NOT NULL,
    PRIMARY KEY (id),
    CONSTRAINT uk_terminal_settings_terminal UNIQUE (terminal_id)
);

CREATE INDEX idx_terminal_settings_terminal ON terminal_settings (terminal_id);
