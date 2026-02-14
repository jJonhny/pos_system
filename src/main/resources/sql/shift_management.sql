-- Shift management and cash drawer reconciliation schema updates.
-- Apply manually in environments that do not rely on JPA schema auto-update.

ALTER TABLE shift
    ADD COLUMN IF NOT EXISTS opened_by VARCHAR(100) NULL,
    ADD COLUMN IF NOT EXISTS closed_by VARCHAR(100) NULL,
    ADD COLUMN IF NOT EXISTS cash_in_total DECIMAL(18,2) NULL,
    ADD COLUMN IF NOT EXISTS cash_out_total DECIMAL(18,2) NULL,
    ADD COLUMN IF NOT EXISTS expected_cash DECIMAL(18,2) NULL,
    ADD COLUMN IF NOT EXISTS variance_cash DECIMAL(18,2) NULL,
    ADD COLUMN IF NOT EXISTS terminal_id VARCHAR(128) NULL,
    ADD COLUMN IF NOT EXISTS close_notes VARCHAR(1000) NULL,
    ADD COLUMN IF NOT EXISTS opening_float_json TEXT NULL,
    ADD COLUMN IF NOT EXISTS counted_amounts_json TEXT NULL,
    ADD COLUMN IF NOT EXISTS expected_amounts_json TEXT NULL,
    ADD COLUMN IF NOT EXISTS variance_amounts_json TEXT NULL;

ALTER TABLE sale
    ADD COLUMN IF NOT EXISTS terminal_id VARCHAR(128) NULL;

CREATE TABLE IF NOT EXISTS shift_cash_event (
    id BIGINT NOT NULL AUTO_INCREMENT,
    shift_id BIGINT NOT NULL,
    created_at DATETIME(6) NOT NULL,
    event_type VARCHAR(32) NOT NULL,
    currency_code VARCHAR(8) NOT NULL,
    amount DECIMAL(18,4) NOT NULL,
    base_amount DECIMAL(18,2) NOT NULL,
    reason VARCHAR(255) NULL,
    actor_username VARCHAR(100) NULL,
    actor_user_id BIGINT NULL,
    terminal_id VARCHAR(128) NULL,
    PRIMARY KEY (id),
    CONSTRAINT fk_shift_cash_event_shift FOREIGN KEY (shift_id) REFERENCES shift (id)
);

CREATE INDEX idx_shift_cash_event_shift ON shift_cash_event (shift_id);
CREATE INDEX idx_shift_cash_event_created_at ON shift_cash_event (created_at);
