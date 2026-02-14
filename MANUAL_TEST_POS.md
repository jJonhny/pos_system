Manual Test Checklist - POS Enhancements

1. Open `/pos` and verify Quick Totals shows Subtotal, Discount, Tax, Total.
2. Add an item from the product grid and verify the cart updates, Quick Totals update immediately, and Recent items panel appears with the item.
3. Click the Recent item "Add" button and verify it re-adds the item and moves to most recent.
4. Add multiple items and verify Recent items caps at 10 and dedupes by SKU (re-adding moves it to the top).
5. Click a cart line (not on inputs/buttons) and press `+` / `-`, then verify quantity updates accordingly.
6. With a cart line selected, press `Del`/`Backspace` and verify the line is removed from the cart.
7. Press `F2` and verify the checkout section scrolls into view and the first button is focused.
8. Press `Ctrl/Cmd+K`, confirm, and verify the cart clears, totals reset, and no console errors.
9. Use the "Clear cart" button in the cart header, confirm, and verify the cart clears and totals reset.
10. In product controls, change `Sort` (Name, Price, Stock) and verify cards reorder instantly on the current page.
11. Use stock filters (`In stock`, `Low stock`, `Out of stock`) and verify visible count updates and empty-state appears when no match.
12. Toggle `Cozy` and `Compact` density and verify card heights/layout adjust while preserving click-to-add behavior.
