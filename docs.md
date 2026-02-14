# Documentation Updates

## Clipboard Image Upload (Products/Categories)

Image upload now supports:

- File picker upload
- Clipboard paste (`Ctrl/Cmd + V`)
- Drag and drop

Supported forms:

- `GET /products/new`
- `GET /products/{id}/edit`
- `GET /categories/new`
- `GET /categories/{id}/edit`

Behavior:

- Pasted/dropped image is assigned to the existing `imageFile` multipart field.
- Preview card is shown before save.
- Clear button resets the pending image selection.

## Upload Size Limits

Multipart limits were increased to avoid frequent 413 errors:

- `spring.servlet.multipart.max-file-size=10MB`
- `spring.servlet.multipart.max-request-size=12MB`

Configured in:

- `src/main/resources/application.properties`
- `src/test/resources/application-test.properties`

## Graceful 413 Handling

Oversized uploads are handled by `UploadExceptionHandler`:

- `src/main/java/com/example/pos_system/config/UploadExceptionHandler.java`

Redirect behavior:

- Product upload overflow -> `/products?error=uploadTooLarge`
- Category upload overflow -> `/categories?error=uploadTooLarge`

UI message:

- `Image is too large. Maximum upload size is 10MB.`

Controller mappings updated:

- `src/main/java/com/example/pos_system/controller/ProductsController.java`
- `src/main/java/com/example/pos_system/controller/CategoriesController.java`

## POS UI Note

Stock filter chip active/inactive styles were corrected on `/pos` so labels remain visible.

## Verification Checklist

1. Open `/products/new` and paste an image.
2. Confirm preview appears, save, and image persists.
3. Open `/categories/new` and test paste/drag-drop.
4. Upload a file larger than 10MB and confirm friendly error (no Whitelabel 413).
5. Open `/pos` and confirm stock filter chips render correctly when active.
