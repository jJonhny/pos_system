# Backend/Frontend Split Proposal

Status: staged proposal with phase-1 in progress.  
Date: February 27, 2026.

## Progress update

Initial migration started (February 27, 2026):

- Currency backend module moved to `modules/currency/{web,application,domain,infrastructure}`.
- Frontend page templates mirrored into `src/main/resources/templates/pages/*` as a non-breaking split start.
- Existing routes and controller view names remain unchanged in this phase.

## Goal

Reduce coupling and improve maintainability by moving from a horizontal package layout
(`controller/service/repository/...`) to a module-first layout that clearly separates backend
domains and frontend assets/templates.

## Current snapshot

Backend currently uses shared horizontal packages under:

- `src/main/java/com/example/pos_system/config`
- `src/main/java/com/example/pos_system/controller`
- `src/main/java/com/example/pos_system/dto`
- `src/main/java/com/example/pos_system/entity`
- `src/main/java/com/example/pos_system/repository`
- `src/main/java/com/example/pos_system/service`

Frontend currently lives in:

- `src/main/resources/templates/*`
- `src/main/resources/static/css/*`
- `src/main/resources/static/js/*`

## Proposed target structure

### Backend (module-first)

```text
src/main/java/com/example/pos_system
  shared/                     # cross-module utilities, common exceptions, shared DTOs
  platform/                   # security, i18n, framework config, infra adapters
  modules/
    auth/
      web/
      application/
      domain/
      infrastructure/
    users/
      web/
      application/
      domain/
      infrastructure/
    catalog/                  # products, categories, variants, units, pricing setup
      web/
      application/
      domain/
      infrastructure/
    inventory/
      web/
      application/
      domain/
      infrastructure/
    purchase/
      web/
      application/
      domain/
      infrastructure/
    sales/
      web/
      application/
      domain/
      infrastructure/
    pos/
      web/
      application/
      domain/
      infrastructure/
    currency/
      web/
      application/
      domain/
      infrastructure/
    reporting/
      web/
      application/
      domain/
      infrastructure/
    audit/
      web/
      application/
      domain/
      infrastructure/
```

### Frontend (page + module assets)

```text
src/main/resources
  templates/
    layouts/                  # base shell/layout templates
    fragments/                # reusable Thymeleaf fragments
    pages/
      auth/
      pos/
      products/
      sales/
      purchases/
      inventory/
      reports/
      admin/
      users/
      currencies/
      audit/
  static/
    css/
      app.css                 # compiled output
    js/
      core/                   # shared utilities (navigation, i18n, htmx helpers)
      modules/                # page/module-specific scripts by domain
      vendor/
```

Optional next phase for frontend source ownership:

```text
src/frontend/
  styles/
  scripts/
```

Build outputs from `src/frontend` would still compile into `src/main/resources/static`.

## Example class mapping

- `ProductsController` -> `modules.catalog.web.ProductsController`
- `ProductVariantService` -> `modules.catalog.application.ProductVariantService`
- `ProductRepo` -> `modules.catalog.infrastructure.ProductRepository`
- `PosController` -> `modules.pos.web.PosController`
- `PosService` -> `modules.pos.application.PosService`
- `SalesController` -> `modules.sales.web.SalesController`
- `SalesService` -> `modules.sales.application.SalesService`
- `AuditEventsController` -> `modules.audit.web.AuditEventsController`

## Migration plan (safe sequence)

1. Create package skeleton (`modules/*`, `platform`, `shared`) without deleting existing packages.
2. Move one low-risk module first (recommended: `currency`), keep URL routes and public APIs unchanged.
3. Move related tests with each module migration; keep integration test names stable.
4. Move template folders to `templates/pages/*` incrementally and keep compatibility redirects/includes.
5. Split JS into `static/js/core` and `static/js/modules`, then update template script references.
6. After all modules are migrated, remove old horizontal packages.

## Non-goals in this proposal

- No change to runtime behavior or endpoint contracts.
- No change to database schema.
- No immediate monorepo or microservice split.
