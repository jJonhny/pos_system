# Variants, Units, and Tier Pricing

This document describes the dynamic attribute/SKU model, multi-unit pricing, and POS integration added in this project.

## Scope

Implemented modules:

- Dynamic attribute groups and values
- Product-level attribute configuration
- Variant generation from cartesian combinations
- Combination exclusion for impossible variants
- Multi-unit sell model (`sku_sell_unit`)
- Unit barcode model (`sku_unit_barcode`)
- Tier prices with customer group overrides (`sku_unit_tier_price`)
- Variant/base-unit inventory tracking (`product_variant.stock_base_qty`, `sku_inventory_balance`)
- POS runtime integration for variant scan, pricing, and checkout deduction

## Database migrations

- `V20260224_01__dynamic_variants_units_pricing.sql`
- `V20260224_02__variant_line_references.sql`

## Core pricing rule

For a given variant + sell unit + quantity:

1. Match active customer-group tier with highest `min_qty`
2. Else match active global tier with highest `min_qty`
3. Else fallback to sell-unit `base_price`

Inventory deduction always uses base units:

`deduct_base_qty = sold_qty * conversion_to_base`

## Main tables

- `attribute_group`, `attribute_value`
- `product_attribute_group`, `product_attribute_value`
- `product_variant`, `product_variant_attribute`, `product_variant_exclusion`
- `unit_of_measure`, `sku_sell_unit`, `sku_unit_barcode`
- `customer_group`, `sku_unit_tier_price`
- `sku_inventory_balance`
- `sale_item` and `held_sale_item` extended with:
  - `variant_id`, `sell_unit_id`, `sell_unit_code`
  - `conversion_to_base`, `price_source`
  - `applied_tier_min_qty`, `applied_tier_group_code`

## API surface

Base path: `/api/v1`

Attributes and variants:

- `POST /attributes/groups`
- `DELETE /attributes/groups/{groupId}`
- `POST /attributes/groups/{groupId}/values`
- `DELETE /attributes/values/{valueId}`
- `PUT /products/{productId}/attribute-config`
- `POST /products/{productId}/variants/generate`
- `POST /products/{productId}/variant-exclusions`
- `DELETE /products/{productId}/variant-exclusions/{exclusionId}`
- `PATCH /variants/{variantId}/state`

Units and pricing:

- `POST /units`
- `POST /customer-groups`
- `POST /variants/{variantId}/sell-units`
- `PUT /sell-units/{sellUnitId}`
- `POST /sell-units/{sellUnitId}/barcodes`
- `PUT /sell-units/{sellUnitId}/tier-prices`
- `POST /pos/pricing/quote`
- `POST /inventory/deduct`

## Example requests

Create unit:

```json
{
  "code": "BOX",
  "name": "Box",
  "precisionScale": 0
}
```

Upsert sell unit:

```json
{
  "unitCode": "BOX",
  "conversionToBase": 24,
  "isBase": false,
  "basePrice": 34.00,
  "enabled": true
}
```

Quote pricing:

```json
{
  "customerGroupCode": "WHOLESALE",
  "currencyCode": "USD",
  "lines": [
    {
      "variantId": 1001,
      "sellUnitId": 5001,
      "qty": 22
    }
  ]
}
```

## POS integration behavior

Integrated endpoints:

- `POST /pos/scan`
- `POST /pos/quick-add`
- `POST /pos/checkout`
- `POST /pos/checkout/split`

Runtime behavior:

- Variant unit barcodes are resolved first in scan/quick-add
- Variant pricing is refreshed from the tier engine when qty/customer changes
- Checkout deducts variant stock in base units (new `VariantInventoryService`)
- Legacy non-variant product flow remains supported in parallel

## Service layer

- `VariantGenerationService`: generation/merge facade (delegates to `ProductVariantService`)
- `PricingService`: tier-selection pricing facade (delegates to `SkuUnitPricingService`)
- `InventoryService`: includes `deductVariantUnitStock(...)` for sell-unit to base-unit deduction

## Variant generation merge behavior

Generation uses canonical combination keys and hash from `VariantCombinationKeyService`:

- Existing rows are matched by `combination_hash`
- Editable fields are preserved by default:
  - `sku`, `barcode`, `price`, `cost`, `stock`, `enabled`, `impossible`
- Missing combinations are archived instead of hard-deleted
- Excluded combinations are skipped and can be marked impossible

## Security notes

- `/api/v1/**` requires admin/manager or inventory authority
- `/api/v1/pos/pricing/quote` is also allowed for POS operator roles (`ADMIN`, `MANAGER`, `CASHIER`, or `PERM_USE_POS`)

## Test references

- `VariantAndPricingServiceIntegrationTest`
- `PosVariantScanAndCheckoutIntegrationTest`
