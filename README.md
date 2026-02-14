# POS System

Production-ready Point of Sale system built with **Spring Boot + Thymeleaf + HTMX**.

## Overview

This project delivers an end-to-end retail workflow:

- fast cashier POS with split/multi-currency checkout
- inventory integrity with stock movement ledger
- supplier receiving (PO + GRN)
- shift/cash-drawer reconciliation
- analytics + report exports
- security, RBAC, and immutable audit logging
- optional local POS bridge for ESC/POS printing and drawer open

---

## Completed Feature Scope

### POS & Checkout

- Product search, category filter, quick-add by SKU/barcode, scanner flow
- Cart with quantity/unit changes, notes, customer attach/create, clear cart
- Cart discount/price/tax override audit coverage
- Hold cart / resume cart
- Split payments and multi-currency tendering
- Idempotent checkout (`clientCheckoutId` + terminal guard) to prevent duplicates
- Receipt page, PDF receipt, and reprint actions
- POS keyboard + usability enhancements (recent/pinned items, quick totals, filters)

### Shift & Cash Drawer

- Open shift with opening float (per currency JSON)
- Cash events during shift: `CASH_IN`, `CASH_OUT`, `DRAWER_OPEN`
- Close shift with counted cash, expected cash, variance, notes
- Manager/admin approval required for high variance (configurable threshold)
- Shift summary reporting + Excel export

### Hardware & Terminal Settings

- Terminal/register management (`/pos-setting`)
- Per-terminal receipt header/footer, tax ID, default currency
- Printer mode: `BRIDGE` or `PDF`
- Bridge print payload endpoint + drawer open endpoint
- Test print and test drawer actions
- Fallback behavior when bridge is unavailable (PDF/manual drawer)
- Optional camera scanner flag per terminal

### Inventory, Products, Categories

- Product CRUD with pricing tiers, stock settings, active toggle
- Category CRUD with sorting and active status
- Bulk stock operations + CSV/XLSX import/export
- Clipboard image paste support (`Ctrl/Cmd + V`) on product/category forms
- Drag/drop image support with preview before save
- Upload overflow handling with user-friendly error (no raw Whitelabel 413)

### Stock Movement Ledger & Integrity

- Unified `stock_movement` ledger (`SALE`, `RETURN`, `VOID`, `RECEIVE`, `ADJUSTMENT`, `IMPORT`, etc.)
- Transactional on-hand updates
- Negative stock blocked unless `allowNegativeStock=true`
- Inventory movements screen + CSV/XLSX export

### Suppliers, Purchase Orders, Goods Receipt

- Supplier CRUD
- Purchase order create/edit/list with line items
- Goods receipt posting (linked/unlinked to PO)
- Automatic PO status progression (`DRAFT`, `SENT`, `PARTIAL`, `RECEIVED`, `CANCELED`)
- Receiving report + Excel export

### Sales, Returns, Voids

- Sales list with filters, sorting, pagination, CSV export
- Bulk actions (void/export/reprint)
- Return flow with refund math and customer point adjustment
- Void flow with stock compensation movements
- Transactional return/void service logic

### Currency & Financial

- Currency CRUD + activate/deactivate
- Base currency switch
- FX refresh from configurable provider
- Currency analytics dashboard (converter + trend/volatility/freshness charts)

### Analytics & Reports

- Main analytics dashboard with KPI cards and operational insights
- Chart.js + Plotly chart coverage (trends, composition, distribution, relationships, advanced gallery)
- Report center:
  - Sales Excel (`/reports/sales.xlsx`)
  - Shift Excel (`/reports/shifts.xlsx`)
  - Inventory ledger Excel (`/reports/inventory-ledger.xlsx`)
  - Receiving Excel (`/reports/receiving.xlsx`)

### Security, Users, Auditing

- Spring Security login flow with access-denied handling
- Roles: `ADMIN`, `MANAGER`, `CASHIER`
- Permission-based overrides (authorities emitted as `PERM_*`)
- User admin: role/status/password/permissions/MFA + bulk actions
- Immutable `audit_event` log for sensitive POS/admin/inventory/currency actions
- Admin audit search UI (`/admin/audit`, alias `/audit-events`)

---

## Architecture

- **Backend:** Spring Boot 4, Spring MVC, Spring Data JPA, Spring Security
- **Frontend:** Thymeleaf templates + HTMX partial swaps + Tailwind CSS
- **Database:** MySQL (runtime), H2 (tests)
- **Charts:** Chart.js + Plotly
- **PDF:** openhtmltopdf

Key principle used across modules:

- business logic in services (`@Transactional` where required)
- thin MVC controllers
- no SPA rewrite (server-rendered pages + HTMX fragments)

---

## Requirements

- JDK 25
- MySQL 8+
- (Optional) Node.js for local POS bridge

---

## Local Setup

1. Create database:

```sql
CREATE DATABASE pos_db;
```

2. Configure DB in `src/main/resources/application.properties`:

```properties
spring.datasource.url=jdbc:mysql://localhost:3306/pos_db?useSSL=false&serverTimezone=UTC
spring.datasource.username=root
spring.datasource.password=
```

3. Run:

```bash
./mvnw spring-boot:run
```

4. Login:

- Admin: `admin` / `admin123`
- Cashier: `cashier` / `cashier123`

> These are seeded only when user table is empty. Change passwords immediately.

---

## Important Configuration

### Currency / FX

```properties
app.currency.base=USD
app.currency.rate-url=https://open.er-api.com/v6/latest/{base}
app.currency.rate-path=rates
app.currency.refresh-ms=3600000
```

### Shift close variance threshold

```properties
app.shift.variance-threshold=20.00
```

Expected cash formula:

`opening float + cash sales + cash in - cash out - cash refunds`

### Multipart upload limits

```properties
spring.servlet.multipart.max-file-size=10MB
spring.servlet.multipart.max-request-size=12MB
```

Oversized uploads are redirected to friendly errors:

- `/products?error=uploadTooLarge`
- `/categories?error=uploadTooLarge`

---

## POS Bridge (Optional Hardware Companion)

Sample bridge implementation:

- `bridge/pos-bridge-node`
- Docs: `bridge/pos-bridge-node/README.md`

Bridge endpoints:

- `POST /print`
- `POST /drawer/open`
- `GET /health`

Main app hardware endpoints:

- `POST /pos/checkout/{saleId}/print`
- `POST /pos/drawer/open`

---

## Routes (Main)

- `/login`
- `/pos`
- `/sales`
- `/products`
- `/categories`
- `/commodity`
- `/marketing`
- `/suppliers`
- `/purchases/po`
- `/inventory/movements`
- `/currencies`
- `/analytics`
- `/reports`
- `/pos-setting`
- `/users`
- `/users/password`
- `/admin/audit` (alias `/audit-events`)

---

## Schema / Migration Scripts

The app currently uses JPA schema update by default. Manual SQL scripts are provided for controlled environments:

- `src/main/resources/sql/audit_events.sql`
- `src/main/resources/sql/checkout_attempts.sql`
- `src/main/resources/sql/shift_management.sql`
- `src/main/resources/sql/inventory_movements_and_purchases.sql`
- `src/main/resources/sql/pos_hardware_terminal_settings.sql`

---

## Testing

Run full test suite:

```bash
./mvnw test
```

Examples of focused runs:

```bash
./mvnw -Dtest=PosAddToCartIntegrationTest test
./mvnw -Dtest=AnalyticsPageIntegrationTest test
./mvnw -Dtest=ImagePasteUploadUiIntegrationTest,UploadTooLargeMessageIntegrationTest test
```

---

## Additional Docs

- `docs.md` – recent implementation notes (clipboard image upload, upload-size handling, POS UI fixes)
- `MANUAL_TEST_POS.md` – manual POS test checklist
- `guidelines.md` – codebase conventions/pattern notes
