# POS System

**Overview**
Spring Boot + Thymeleaf point‑of‑sale application with inventory, sales tracking, multi‑currency payments, and analytics.

**Features**
- POS checkout with cart discounts (fixed amount or percent), split payments, and optional reason
- Multi‑currency tendering with live FX refresh and currency management
- Inventory management for products and categories, low‑stock insights
- Sales history with returns, receipts, and PDF export
- Reports with Excel export
- Analytics dashboard (sales trends, payment mix, inventory health, customers, staff, compliance)
- User management with roles (ADMIN, MANAGER, CASHIER) and fine‑grained permissions

**Tech Stack**
- Codex from ChatGPT
- Java 25, Spring Boot 4.0.2
- Spring MVC + Thymeleaf
- Spring Data JPA + MySQL
- Spring Security
- Tailwind CSS + Chart.js
- openhtmltopdf (receipt PDFs)

**Requirements**
- JDK 25
- MySQL 8+

**Setup**
1. Create the database.

```sql
CREATE DATABASE pos_db;
```

2. Update database credentials in `src/main/resources/application.properties`.

```
spring.datasource.url=jdbc:mysql://localhost:3306/pos_db?useSSL=false&serverTimezone=UTC
spring.datasource.username=root
spring.datasource.password=
```

3. Run the application.

```bash
./mvnw spring-boot:run
```

**Currency & FX**
Currency settings are configured in `src/main/resources/application.properties`:

```
app.currency.base=USD
app.currency.rate-url=https://open.er-api.com/v6/latest/{base}
app.currency.rate-path=rates
app.currency.refresh-ms=3600000
```

The rate URL should return a JSON object with a `rates` map keyed by currency code. The default free endpoint is rate‑limited and typically updates once per day.

**Default Users**
- Admin: `admin` / `admin123`
- Cashier: `cashier` / `cashier123`

These are created only when the user table is empty. You can override them using properties:
- `app.seed.admin.username`
- `app.seed.admin.password`
- `app.seed.cashier.username`
- `app.seed.cashier.password`

**Dev Sample Data**
Run with the `dev` profile to seed sample categories, products, and a sample sale.

```bash
SPRING_PROFILES_ACTIVE=dev ./mvnw spring-boot:run
```

**Key Routes**
- `http://localhost:8080/login`
- `http://localhost:8080/pos`
- `http://localhost:8080/products`
- `http://localhost:8080/categories`
- `http://localhost:8080/sales`
- `http://localhost:8080/analytics`
- `http://localhost:8080/reports`
- `http://localhost:8080/users`
- `http://localhost:8080/currencies`

**Access Control**
- ADMIN: full access
- MANAGER: analytics, reports, inventory, sales
- CASHIER: POS and receipts only
- Permissions can grant access to specific modules beyond roles.

Details are in `src/main/java/com/example/pos_system/config/SecurityConfig.java`.

**Security Note**
Passwords are stored with BCrypt. Legacy `{noop}` (or plain) passwords are supported and upgraded on login.
