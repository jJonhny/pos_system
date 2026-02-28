# POS Admin React (Separate Frontend)

This is a standalone React Admin frontend for the existing POS backend.

- It **does not replace** the current Spring MVC + Thymeleaf admin.
- Existing MVC pages remain available on `http://localhost:8080/*`.
- This app consumes REST APIs under `http://localhost:8080/api/v1/*`.

## 1) Prerequisites

- Node.js 18+ and npm
- POS backend running on port `8080`

## 2) Setup

```bash
cd admin-react
cp .env.example .env
npm install
```

## 3) Run Dev Server

```bash
npm run dev
```

Default frontend URL: `http://localhost:5173`

## 4) Production Build

```bash
npm run build
npm run preview
```

## 5) API Wiring Included

- Auth:
  - `POST /api/v1/auth/login`
  - `POST /api/v1/auth/verify-otp`
  - `GET /api/v1/auth/me`
- Users:
  - `GET /api/v1/users`
  - `POST /api/v1/users`
  - `PATCH /api/v1/users/{id}/role`
  - `PATCH /api/v1/users/{id}/status`
  - `PUT /api/v1/users/{id}/permissions`
- Products:
  - `GET /api/v1/products`
  - `POST /api/v1/products`
  - `PUT /api/v1/products/{id}`
- Inventory:
  - `GET /api/v1/inventory/movements`
  - `GET /api/v1/inventory/products/{productId}/availability`
  - `POST /api/v1/inventory/adjustments`
  - `POST /api/v1/inventory/receive`
- Suppliers:
  - `GET /api/v1/suppliers`
  - `GET /api/v1/suppliers/{id}`
  - `POST /api/v1/suppliers`
  - `PUT /api/v1/suppliers/{id}`
  - `DELETE /api/v1/suppliers/{id}`
- Reports:
  - `GET /api/v1/reports/summary`
  - `GET /api/v1/reports/sales`
  - `GET /api/v1/reports/shifts`
- Audit:
  - `GET /api/v1/audit/meta`
  - `GET /api/v1/audit/events`

## 6) Notes

- Product creation in React assumes initial stock is `0`.
- Stock changes should happen through inventory receive/adjust APIs.
- Roles are enforced in frontend routing and validated again by backend security.
