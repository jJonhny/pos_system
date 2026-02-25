# POS System

Enterprise-oriented Point of Sale system built with Spring Boot MVC, Thymeleaf, and HTMX.

## What this project includes

- Full cashier POS workflow (search, barcode scan, cart, hold/resume, checkout)
- Dynamic product attributes and SKU variants (Taobao-style matrix generation)
- Multi-unit SKU model (piece/box/carton/pack) with base-unit inventory conversion
- Tier pricing with customer-group priority (`group tier > global tier > base unit price`)
- Multi-currency cash tender + split payment checkout
- Shift management (open, cash in/out, close, variance checks)
- Sales, return, void, receipt and PDF receipt flows
- Product/category/supplier/purchase/receiving/inventory modules
- Analytics and reporting pages with export endpoints
- Role + permission based security model
- Audit logging for sensitive actions
- Optional local bridge integration for printer and cash drawer

## Internationalization (i18n)

Single codebase, multilingual support:

- Supported locales: `en`, `zh-CN`
- Default locale: `en`
- Fallback behavior: `zh-CN -> zh -> en`
- Message bundles:
  - `src/main/resources/messages.properties`
  - `src/main/resources/messages_en.properties`
  - `src/main/resources/messages_zh.properties`
  - `src/main/resources/messages_zh_CN.properties`
- Locale switching via query parameter: `?lang=en` or `?lang=zh-CN`
- Preference persistence:
  - logged-in users: persisted in `app_user.language_preference`
  - non-logged-in context: locale cookie (`POS_LANG`)
- Receipt/PDF localization uses the locale captured at payment time (`sale.receiptLocale`)

## Architecture

- Backend: Spring Boot `4.0.2`, Spring MVC, Spring Data JPA, Spring Security
- View: Thymeleaf templates + HTMX partial updates
- Styling: Tailwind CSS
- DB: MySQL (runtime), H2 (tests)
- DB migration: Flyway (`src/main/resources/db/migration`)
- PDF rendering: OpenHTMLtoPDF
- Build: Maven Wrapper (`./mvnw`)

Core principles:

- transactional business logic in services
- thin controllers
- server-rendered UI (no SPA rewrite)

## Key POS capabilities

- Independent product/cart scrolling (desktop split layout)
- Infinite product feed with cursor pagination (`/pos/products/feed`)
- Request lock + de-dup + retry handling in UI flow
- Product filters/search that reset cursor correctly
- Cart totals and checkout actions optimized for fast operator flow
- Variant barcode scan + variant SKU quick add (`/pos/scan`, `/pos/quick-add`)
- Variant-aware POS pricing via unit + tier engine (`/api/v1/pos/pricing/quote` internally and API)
- Variant sale checkout consumes inventory in base units and stores variant/unit references on sale lines

## Prerequisites

- Java 25
- MySQL 8+
- Node.js (required for Tailwind CSS build; also used for optional local bridge)

## Local setup

1. Create DB:

```sql
CREATE DATABASE pos_db;
```

2. Configure datasource in `src/main/resources/application.properties`:

```properties
spring.datasource.url=jdbc:mysql://localhost:3306/pos_db?useSSL=false&serverTimezone=UTC
spring.datasource.username=root
spring.datasource.password=
```

3. Install frontend dependencies and build Tailwind CSS:

```bash
npm install
npm run build:css
```

For active frontend development:

```bash
npm run watch:css
```

4. Run application:

```bash
./mvnw spring-boot:run
```

Flyway migrations run automatically on startup (`spring.flyway.enabled=true`).

5. Default seeded users (when user table is empty):

- `admin / admin123`
- `cashier / cashier123`

Change credentials immediately in non-dev environments.

## Important configuration

### Currency/FX

```properties
app.currency.base=USD
app.currency.rate-url=https://open.er-api.com/v6/latest/{base}
app.currency.rate-path=rates
app.currency.refresh-ms=3600000
app.currency.refresh-initial-ms=5000
```

### Shift variance control

```properties
app.shift.variance-threshold=20.00
```

### Product feed / cursor pagination

```properties
app.pagination.cursor-secret=${APP_PAGINATION_CURSOR_SECRET:change-me-in-prod}
app.pagination.feed.rate-limit-per-minute=240
```

### Upload limits

```properties
spring.servlet.multipart.max-file-size=10MB
spring.servlet.multipart.max-request-size=12MB
```

### Flyway migration

```properties
spring.flyway.enabled=true
spring.flyway.baseline-on-migrate=true
spring.flyway.locations=classpath:db/migration
```

## Optional POS bridge

Reference implementation:

- `bridge/pos-bridge-node/README.md`

Bridge endpoints:

- `POST /print`
- `POST /drawer/open`
- `GET /health`

Main app hardware endpoints:

- `POST /pos/checkout/{saleId}/print`
- `POST /pos/drawer/open`

## Main routes

- `/login`
- `/pos`
- `/sales`
- `/products`
- `/categories`
- `/commodity`
- `/suppliers`
- `/purchases/po`
- `/inventory/movements`
- `/currencies`
- `/analytics`
- `/reports`
- `/pos-setting`
- `/users`
- `/admin/audit` (alias `/audit-events`)

## Auth API (Email + TOTP + JWT)

New auth endpoints:

- `POST /api/v1/auth/register`
- `POST /api/v1/auth/login`
- `POST /api/v1/auth/verify-otp`
- `GET /api/v1/auth/me` (requires Bearer token after OTP verification)

Flow:

1. Register with email + password (`bcrypt` hashing via app password encoder).
2. Login with email + password.
3. If first login, backend returns TOTP setup payload (`otpauthUrl` + QR data URL) generated via `speakeasy`.
4. Verify OTP code.
5. Receive JWT access token.

Role set supported by auth API:

- `SUPER_ADMIN`
- `BRANCH_MANAGER`
- `CASHIER`
- `INVENTORY_STAFF`

## Variant and pricing APIs

Admin / inventory routes:

- `POST /api/v1/attributes/groups`
- `POST /api/v1/attributes/groups/{groupId}/values`
- `PUT /api/v1/products/{productId}/attribute-config`
- `POST /api/v1/products/{productId}/variants/generate`
- `POST /api/v1/products/{productId}/variant-exclusions`
- `PATCH /api/v1/variants/{variantId}/state`
- `POST /api/v1/units`
- `POST /api/v1/customer-groups`
- `POST /api/v1/variants/{variantId}/sell-units`
- `PUT /api/v1/sell-units/{sellUnitId}/tier-prices`
- `POST /api/v1/inventory/deduct`

POS pricing quote route:

- `POST /api/v1/pos/pricing/quote`

See [docs/variants-units-pricing.md](docs/variants-units-pricing.md) for full details, examples, and integration notes.

## Database resources

Flyway migrations:

- `src/main/resources/db/migration/V20260224_01__dynamic_variants_units_pricing.sql`
- `src/main/resources/db/migration/V20260224_02__variant_line_references.sql`

Legacy SQL scripts (controlled/manual environments):

- `src/main/resources/sql/audit_events.sql`
- `src/main/resources/sql/checkout_attempts.sql`
- `src/main/resources/sql/shift_management.sql`
- `src/main/resources/sql/inventory_movements_and_purchases.sql`
- `src/main/resources/sql/pos_hardware_terminal_settings.sql`

## Testing

Run full suite:

```bash
./mvnw test
```

Run focused suites:

```bash
./mvnw -Dtest=I18nMessageSourceIntegrationTest,I18nLocaleSwitchIntegrationTest,ReceiptLocaleIntegrationTest,ReceiptPdfServiceIntegrationTest test
./mvnw -Dtest=PosCashTenderIntegrationTest,PosPrintPayloadIntegrationTest,ReceiptPayloadServiceTests test
./mvnw -Dtest=VariantAndPricingServiceIntegrationTest,PosVariantScanAndCheckoutIntegrationTest test
```

Compile check:

```bash
./mvnw -DskipTests compile
```
