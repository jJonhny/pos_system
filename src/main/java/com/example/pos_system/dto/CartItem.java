package com.example.pos_system.dto;

import com.example.pos_system.entity.PriceTier;
import com.example.pos_system.entity.UnitType;

import java.math.BigDecimal;
import java.math.RoundingMode;

public class CartItem {
    private Long productId;
    private String name;
    private BigDecimal unitPrice;
    private int qty;
    private String note;
    private PriceTier priceTier = PriceTier.RETAIL;
    private UnitType unitType = UnitType.PIECE;
    private Integer unitSize = 1;
    private Long variantId;
    private Long sellUnitId;
    private String sellUnitCode;
    private BigDecimal conversionToBase = BigDecimal.ONE;
    private String priceSource;
    private BigDecimal appliedTierMinQty;
    private String appliedTierGroupCode;

    /**
     * Executes the CartItem operation.
     * <p>Return value: A fully initialized CartItem instance.</p>
     *
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public CartItem() {}

    /**
     * Executes the CartItem operation.
     * <p>Return value: A fully initialized CartItem instance.</p>
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param unitPrice Parameter of type {@code BigDecimal} used by this operation.
     * @param qty Parameter of type {@code int} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public CartItem(Long productId, String name, BigDecimal unitPrice, int qty) {
        this(productId, name, unitPrice, qty, PriceTier.RETAIL, UnitType.PIECE, 1);
    }

    /**
     * Executes the CartItem operation.
     * <p>Return value: A fully initialized CartItem instance.</p>
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param name Parameter of type {@code String} used by this operation.
     * @param unitPrice Parameter of type {@code BigDecimal} used by this operation.
     * @param qty Parameter of type {@code int} used by this operation.
     * @param priceTier Parameter of type {@code PriceTier} used by this operation.
     * @param unitType Parameter of type {@code UnitType} used by this operation.
     * @param unitSize Parameter of type {@code int} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public CartItem(Long productId, String name, BigDecimal unitPrice, int qty, PriceTier priceTier,
                    UnitType unitType, int unitSize) {
        this.productId = productId;
        this.name = name;
        this.unitPrice = unitPrice;
        this.qty = qty;
        this.priceTier = priceTier == null ? PriceTier.RETAIL : priceTier;
        this.unitType = unitType == null ? UnitType.PIECE : unitType;
        this.unitSize = unitSize <= 0 ? 1 : unitSize;
    }

    /**
     * Executes the getLineTotal operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getLineTotal() {
        if (unitPrice == null) return BigDecimal.ZERO;
        return unitPrice.multiply(BigDecimal.valueOf(qty));
    }

    /**
     * Executes the getProductId operation.
     *
     * @return {@code Long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Long getProductId() { return productId; }
    /**
     * Executes the setProductId operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setProductId(Long productId) { this.productId = productId; }

    /**
     * Executes the getName operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String getName() { return name; }
    /**
     * Executes the setName operation.
     *
     * @param name Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setName(String name) { this.name = name; }

    /**
     * Executes the getUnitPrice operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getUnitPrice() { return unitPrice; }
    /**
     * Executes the setUnitPrice operation.
     *
     * @param unitPrice Parameter of type {@code BigDecimal} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setUnitPrice(BigDecimal unitPrice) { this.unitPrice = unitPrice; }

    /**
     * Executes the getQty operation.
     *
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public int getQty() { return qty; }
    /**
     * Executes the setQty operation.
     *
     * @param qty Parameter of type {@code int} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setQty(int qty) { this.qty = qty; }

    /**
     * Executes the getNote operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String getNote() { return note; }
    /**
     * Executes the setNote operation.
     *
     * @param note Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setNote(String note) { this.note = note; }

    /**
     * Executes the getPriceTier operation.
     *
     * @return {@code PriceTier} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public PriceTier getPriceTier() {
        return priceTier == null ? PriceTier.RETAIL : priceTier;
    }
    /**
     * Executes the setPriceTier operation.
     *
     * @param priceTier Parameter of type {@code PriceTier} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setPriceTier(PriceTier priceTier) {
        this.priceTier = priceTier == null ? PriceTier.RETAIL : priceTier;
    }

    /**
     * Executes the getUnitType operation.
     *
     * @return {@code UnitType} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public UnitType getUnitType() {
        return unitType == null ? UnitType.PIECE : unitType;
    }
    /**
     * Executes the setUnitType operation.
     *
     * @param unitType Parameter of type {@code UnitType} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setUnitType(UnitType unitType) {
        this.unitType = unitType == null ? UnitType.PIECE : unitType;
    }

    /**
     * Executes the getUnitSize operation.
     *
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public int getUnitSize() {
        return unitSize == null || unitSize <= 0 ? 1 : unitSize;
    }
    /**
     * Executes the setUnitSize operation.
     *
     * @param unitSize Parameter of type {@code Integer} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setUnitSize(Integer unitSize) {
        this.unitSize = unitSize == null || unitSize <= 0 ? 1 : unitSize;
    }

    /**
     * Executes the getEffectiveQty operation.
     *
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public int getEffectiveQty() {
        return getUnitSize() * qty;
    }

    /**
     * Executes the isVariantLine operation.
     *
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public boolean isVariantLine() {
        return variantId != null && sellUnitId != null;
    }

    /**
     * Executes the getConversionToBase operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getConversionToBase() {
        if (conversionToBase == null || conversionToBase.compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ONE;
        }
        return conversionToBase;
    }
    /**
     * Executes the setConversionToBase operation.
     *
     * @param conversionToBase Parameter of type {@code BigDecimal} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setConversionToBase(BigDecimal conversionToBase) {
        if (conversionToBase == null || conversionToBase.compareTo(BigDecimal.ZERO) <= 0) {
            this.conversionToBase = BigDecimal.ONE;
        } else {
            this.conversionToBase = conversionToBase;
        }
    }

    /**
     * Executes the getEffectiveBaseQty operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getEffectiveBaseQty() {
        BigDecimal qtyValue = BigDecimal.valueOf(Math.max(0, qty));
        if (isVariantLine()) {
            return getConversionToBase().multiply(qtyValue).setScale(6, RoundingMode.HALF_UP);
        }
        return BigDecimal.valueOf((long) getEffectiveQty()).setScale(6, RoundingMode.HALF_UP);
    }

    /**
     * Executes the getVariantId operation.
     *
     * @return {@code Long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Long getVariantId() { return variantId; }
    /**
     * Executes the setVariantId operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setVariantId(Long variantId) { this.variantId = variantId; }

    /**
     * Executes the getSellUnitId operation.
     *
     * @return {@code Long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Long getSellUnitId() { return sellUnitId; }
    /**
     * Executes the setSellUnitId operation.
     *
     * @param sellUnitId Parameter of type {@code Long} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setSellUnitId(Long sellUnitId) { this.sellUnitId = sellUnitId; }

    /**
     * Executes the getSellUnitCode operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String getSellUnitCode() { return sellUnitCode; }
    /**
     * Executes the setSellUnitCode operation.
     *
     * @param sellUnitCode Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setSellUnitCode(String sellUnitCode) { this.sellUnitCode = sellUnitCode; }

    /**
     * Executes the getPriceSource operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String getPriceSource() { return priceSource; }
    /**
     * Executes the setPriceSource operation.
     *
     * @param priceSource Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setPriceSource(String priceSource) { this.priceSource = priceSource; }

    /**
     * Executes the getAppliedTierMinQty operation.
     *
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public BigDecimal getAppliedTierMinQty() { return appliedTierMinQty; }
    /**
     * Executes the setAppliedTierMinQty operation.
     *
     * @param appliedTierMinQty Parameter of type {@code BigDecimal} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setAppliedTierMinQty(BigDecimal appliedTierMinQty) { this.appliedTierMinQty = appliedTierMinQty; }

    /**
     * Executes the getAppliedTierGroupCode operation.
     *
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public String getAppliedTierGroupCode() { return appliedTierGroupCode; }
    /**
     * Executes the setAppliedTierGroupCode operation.
     *
     * @param appliedTierGroupCode Parameter of type {@code String} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void setAppliedTierGroupCode(String appliedTierGroupCode) { this.appliedTierGroupCode = appliedTierGroupCode; }
}
