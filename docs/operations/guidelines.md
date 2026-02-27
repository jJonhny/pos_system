# Codebase Guidelines (POS System)

Last reviewed: 2026-02-14

## 1) Purpose and Stack
- Application type: monolithic Spring Boot + Thymeleaf POS app.
- Primary capabilities: POS checkout, inventory management, sales/returns, reporting, analytics, user/role management, multi-currency tendering.
- Core stack:
- Java 25
- Spring Boot 4.0.2
- Spring MVC + Thymeleaf
- Spring Data JPA + MySQL
- Spring Security
- HTMX + Tailwind (via CDN)
- Chart.js + openhtmltopdf

## 2) High-Level Structure
- Backend root: `src/main/java/com/example/pos_system`
- Frontend templates: `src/main/resources/templates`
- Static assets: `src/main/resources/static`
- Config: `src/main/resources/application.properties`

Layer map:
- `controller/`: request handling + significant orchestration/business logic.
- `service/`: mixed depth services. `PosService` and `CurrencyService` are meaningful service boundaries.
- `repository/`: thin Spring Data interfaces with a few custom queries/specifications.
- `entity/`: JPA entities with Lombok `@Getter/@Setter`.
- `dto/`: cart and analytics DTOs.
- `config/`: security, seeders, global model attrs, password encoder.
- `util/`: UI formatting helper (`UiFormat`).

Current size hotspots (important for future edits):
- `src/main/java/com/example/pos_system/controller/ProductsController.java` (very large)
- `src/main/java/com/example/pos_system/controller/PosController.java` (very large)
- `src/main/java/com/example/pos_system/controller/SalesController.java` (large)
- `src/main/java/com/example/pos_system/service/DashboardService.java` (very large)
- `src/main/resources/templates/pos/index.html` (very large, large inline JS)
- `src/main/resources/templates/analytics.html` (very large)

## 3) Architectural Patterns in This Repo

### 3.1 Controller-centric orchestration
- Controllers handle:
- input parsing/validation
- model shaping
- many business rules
- branching for full-page vs HTMX partial responses
- Use this style only when making small/targeted changes. For new non-trivial logic, prefer extracting to service classes to avoid further controller growth.

### 3.2 HTMX partial rendering for POS
- POS page uses server-rendered fragments from `src/main/resources/templates/pos/fragments.html`.
- `PosController` returns either full page (`pos/index`) or fragments depending on `HX-Request`.
- UI behaviors in `pos/index.html` script depend on stable IDs:
- `#productGridWrap`
- `#cartContainer`
- `#cartPanel`
- `#checkoutSection`
- Do not rename/remove these IDs without updating both Thymeleaf fragments and JS handlers.

### 3.3 Session-scoped cart
- POS cart is session state via `@SessionAttributes("cart")` in `PosController`.
- Source of truth for totals:
- `src/main/java/com/example/pos_system/dto/Cart.java`
- `subtotal -> discount -> tax -> total` logic lives here.
- Never re-implement total math in templates/JS/backend endpoints. Reuse `Cart` values.

### 3.4 Multi-currency model
- Currency rates are persisted and refreshed by `CurrencyService`.
- Base currency in DB, with conversion via `rateToBase`.
- Cash/split payments can capture:
- base amount
- foreign amount
- currency code and rate
- Keep payment conversion centralized in POS controller helpers and service calls.

### 3.5 Security model
- Route access mostly in `SecurityConfig` with role + authority expressions.
- AppUser authorities format:
- roles: `ROLE_*`
- permissions: `PERM_*`
- Password encoder supports legacy plaintext/noop and upgrades to bcrypt.

## 4) Frontend Patterns
- Styling is mostly utility-first Tailwind directly in templates.
- Shared nav/header is duplicated across templates, not extracted to a common Thymeleaf layout.
- Significant page logic is inline JS (especially POS and analytics pages).
- `src/main/resources/static/js/pos.js` and `src/main/resources/static/css/app.css` are currently empty.
- Local browser storage is used in POS for UI state:
- pinned items
- recent items
- product listing preferences
- cash currency preference

Guideline:
- For POS-only interaction state, continue using `localStorage` + resilient fallbacks.
- For server-authoritative business state, keep source of truth in backend/session.

## 5) Data and Persistence Patterns
- JPA entities use mostly default mappings; some useful indexes are present on `Product`.
- Repositories are mostly straightforward.
- Query style varies:
- some proper DB filtering via specs/queries
- some in-memory filtering after `findAll(...)` (sales/reports/analytics)

Guideline:
- Prefer DB-side filtering/pagination for new heavy queries.
- Avoid loading full tables in memory for list endpoints when growth is expected.

## 6) Testing State
- Test coverage is minimal (`contextLoads` only).
- Running `./mvnw test` currently requires live MySQL and fails when DB is unavailable.

Guideline:
- New features should include at least one focused test where possible.
- For controller/service logic, prefer slice or unit tests that do not require a live external DB.
- Introduce a dedicated test profile (e.g., H2/Testcontainers) before expanding test suite broadly.

## 7) Key Technical Risks Observed
- Very large controller/template files increase regression risk and reduce maintainability.
- Domain logic duplicated across controller methods and UI script flows.
- In-memory filtering/aggregation on potentially large datasets (`SaleRepo.findAll()` paths).
- `application.properties` has duplicate currency provider keys.
- Error verbosity is high in default config (`include-stacktrace=always`) and should not be production default.
- Runtime image upload writes into `src/main/resources/static/uploads` and mirrors into `target/classes`; operationally fragile for immutable deployments.

## 8) Engineering Guidelines for Future Work

### 8.1 Business logic placement
- Keep controllers focused on:
- request mapping
- validation
- response/fragment selection
- Move reusable rules/calculation branches into services.
- If adding >40-60 lines of new logic in a controller, extract it.

### 8.2 Monetary calculations
- Use `BigDecimal` end-to-end.
- Define rounding mode at boundaries only (display/export/payment persistence boundaries).
- Reuse `Cart` methods for subtotal/discount/tax/total.
- Never compute totals in JS as the source of truth.

### 8.3 POS page changes
- Preserve HTMX fragment contract and IDs.
- Keep keyboard shortcuts scoped:
- only active on POS page
- disabled while typing in input/textarea/select/contenteditable
- Ensure event listeners have deterministic behavior after HTMX swaps.
- Prefer enhancing existing `afterCartSwap()` and helper functions rather than adding parallel initialization paths.

### 8.4 Template JS hygiene
- If adding significant JS behavior, extract into `src/main/resources/static/js/pos.js` and keep inline script as bootstrapping only.
- Keep helper functions pure where possible.
- Avoid anonymous duplicated event handlers across HTMX and fallback paths.

### 8.5 Repository/query hygiene
- For lists with filters + paging, use repository/specification queries.
- Avoid `findAll()` + stream filtering for new endpoints unless dataset is guaranteed small.
- For analytics, consider dedicated projection queries or pre-aggregation if data grows.

### 8.6 Error handling
- Continue using user-friendly flash/model messages.
- For HTMX errors, return relevant fragment with message in model.
- Do not leak stack traces to end users in production configuration.

### 8.7 Security and authorization
- Every new route must be evaluated in `SecurityConfig`.
- Follow existing permission naming pattern (`PERM_*`).
- Preserve self-protection rules in user management flows.

### 8.8 Uploads/files
- Validate content type and path handling.
- Prefer externalized upload directory for production deployments.
- Do not rely on writable classpath at runtime outside local dev.

### 8.9 Seeders and environments
- Keep sample data under `dev` profile only.
- Seed defaults only when tables are empty.
- Document any new seed credentials/settings in README and properties.

## 9) Practical Conventions
- Use `@Transactional` on service methods that mutate multiple entities.
- Keep enum-backed fields as `@Enumerated(EnumType.STRING)`.
- Use `UiFormat` for template money/date formatting.
- Keep redirects and query param preservation consistent with existing list pages.
- Maintain Tailwind visual language already in use unless a page-wide redesign is intentional.

## 10) Preferred Refactor Sequence (when time allows)
1. Extract POS page script logic to `static/js/pos.js` in modules (cart shortcuts, product grid state, recents/pins, tendering).
2. Split oversized controllers (`PosController`, `ProductsController`, `SalesController`) into coordinator + service helpers.
3. Replace in-memory list filtering in sales/reports with repository-level pagination/filtering.
4. Introduce test profile and add service/controller tests for checkout, returns, and permission-sensitive flows.
5. Create shared Thymeleaf layout/fragments for repeated header/nav.

## 11) Pre-Merge Checklist for New Changes
- Build compiles and app starts.
- Security rules updated for new endpoints.
- POS HTMX fragments still swap correctly.
- Cart totals remain server-authoritative and correct under add/remove/discount/tax/checkout.
- Keyboard shortcuts (if touched) do not interfere with text inputs.
- No new N+1 or table-scan-heavy query paths introduced unintentionally.
- Manual smoke test performed for affected modules.
