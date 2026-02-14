-- Stock movement ledger + supplier receiving (PO/GRN)
-- Apply manually in environments that do not rely on JPA schema auto-update.

ALTER TABLE product
    ADD COLUMN IF NOT EXISTS allow_negative_stock BIT(1) NOT NULL DEFAULT b'0';

CREATE TABLE IF NOT EXISTS stock_movement (
    id BIGINT NOT NULL AUTO_INCREMENT,
    product_id BIGINT NOT NULL,
    qty_delta INT NOT NULL,
    unit_cost DECIMAL(18,4) NULL,
    currency VARCHAR(8) NULL,
    movement_type VARCHAR(24) NOT NULL,
    ref_type VARCHAR(32) NULL,
    ref_id VARCHAR(128) NULL,
    created_at DATETIME(6) NOT NULL,
    actor_user_id BIGINT NULL,
    terminal_id VARCHAR(128) NULL,
    notes VARCHAR(500) NULL,
    PRIMARY KEY (id),
    CONSTRAINT fk_stock_movement_product FOREIGN KEY (product_id) REFERENCES product (id),
    CONSTRAINT chk_stock_movement_sale_delta CHECK (movement_type <> 'SALE' OR qty_delta < 0)
);

CREATE INDEX idx_stock_movement_product_created ON stock_movement (product_id, created_at);
CREATE INDEX idx_stock_movement_type_created ON stock_movement (movement_type, created_at);

CREATE TABLE IF NOT EXISTS supplier (
    id BIGINT NOT NULL AUTO_INCREMENT,
    name VARCHAR(180) NOT NULL,
    phone VARCHAR(60) NULL,
    email VARCHAR(180) NULL,
    address VARCHAR(500) NULL,
    status VARCHAR(20) NOT NULL,
    PRIMARY KEY (id)
);

CREATE INDEX idx_supplier_name ON supplier (name);
CREATE INDEX idx_supplier_status ON supplier (status);

CREATE TABLE IF NOT EXISTS purchase_order (
    id BIGINT NOT NULL AUTO_INCREMENT,
    supplier_id BIGINT NOT NULL,
    status VARCHAR(20) NOT NULL,
    created_at DATETIME(6) NOT NULL,
    created_by_user_id BIGINT NULL,
    created_by VARCHAR(100) NULL,
    expected_at DATE NULL,
    notes VARCHAR(1000) NULL,
    currency VARCHAR(8) NULL,
    PRIMARY KEY (id),
    CONSTRAINT fk_purchase_order_supplier FOREIGN KEY (supplier_id) REFERENCES supplier (id)
);

CREATE INDEX idx_purchase_order_status_created ON purchase_order (status, created_at);

CREATE TABLE IF NOT EXISTS purchase_order_item (
    id BIGINT NOT NULL AUTO_INCREMENT,
    po_id BIGINT NOT NULL,
    product_id BIGINT NOT NULL,
    ordered_qty INT NOT NULL,
    received_qty INT NOT NULL,
    unit_cost DECIMAL(18,4) NOT NULL,
    tax DECIMAL(18,4) NULL,
    discount DECIMAL(18,4) NULL,
    PRIMARY KEY (id),
    CONSTRAINT fk_purchase_order_item_po FOREIGN KEY (po_id) REFERENCES purchase_order (id),
    CONSTRAINT fk_purchase_order_item_product FOREIGN KEY (product_id) REFERENCES product (id)
);

CREATE INDEX idx_purchase_order_item_po ON purchase_order_item (po_id);
CREATE INDEX idx_purchase_order_item_product ON purchase_order_item (product_id);

CREATE TABLE IF NOT EXISTS goods_receipt (
    id BIGINT NOT NULL AUTO_INCREMENT,
    po_id BIGINT NULL,
    received_at DATETIME(6) NOT NULL,
    received_by_user_id BIGINT NULL,
    received_by VARCHAR(100) NULL,
    invoice_no VARCHAR(120) NULL,
    notes VARCHAR(1000) NULL,
    PRIMARY KEY (id),
    CONSTRAINT fk_goods_receipt_po FOREIGN KEY (po_id) REFERENCES purchase_order (id)
);

CREATE INDEX idx_goods_receipt_received_at ON goods_receipt (received_at);

CREATE TABLE IF NOT EXISTS goods_receipt_item (
    id BIGINT NOT NULL AUTO_INCREMENT,
    grn_id BIGINT NOT NULL,
    product_id BIGINT NOT NULL,
    received_qty INT NOT NULL,
    unit_cost DECIMAL(18,4) NOT NULL,
    PRIMARY KEY (id),
    CONSTRAINT fk_goods_receipt_item_grn FOREIGN KEY (grn_id) REFERENCES goods_receipt (id),
    CONSTRAINT fk_goods_receipt_item_product FOREIGN KEY (product_id) REFERENCES product (id)
);

CREATE INDEX idx_goods_receipt_item_grn ON goods_receipt_item (grn_id);
CREATE INDEX idx_goods_receipt_item_product ON goods_receipt_item (product_id);
