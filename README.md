# POS System

Enterprise-oriented Point of Sale system built with Spring Boot MVC, Thymeleaf, and HTMX.

## What this project includes

- Full cashier POS workflow (search, barcode scan, cart, hold/resume, checkout)
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

## Prerequisites

- Java 25
- MySQL 8+
- Node.js (optional, only for local bridge)

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

3. Run application:

```bash
./mvnw spring-boot:run
```

4. Default seeded users (when user table is empty):

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

## SQL scripts (for controlled environments)

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
```

Compile check:

```bash
./mvnw -DskipTests compile
```
