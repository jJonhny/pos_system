# POS System

**Overview**
Spring Boot + Thymeleaf point‑of‑sale application with inventory management, sales tracking, and an analytics dashboard.

**Features**
- POS checkout with cart discounts (fixed amount or percent) and optional reason
- Inventory management for products and categories
- Sales history with returns, receipts, and PDF export
- Admin analytics dashboard
- Role‑based access control (ADMIN, CASHIER)

**Tech Stack**
- Java 25, Spring Boot 4.0.2
- Spring MVC + Thymeleaf
- Spring Data JPA + MySQL
- Spring Security
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

**Access Control**
- ADMIN: full access
- CASHIER: POS and receipts only

Details are in `src/main/java/com/example/pos_system/config/SecurityConfig.java`.

**Security Note**
Passwords are stored with `NoOpPasswordEncoder` for development. Do not use this in production.
