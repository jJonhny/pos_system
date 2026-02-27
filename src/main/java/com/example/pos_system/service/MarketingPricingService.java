package com.example.pos_system.service;

import com.example.pos_system.dto.Cart;
import com.example.pos_system.entity.Customer;
import com.example.pos_system.entity.DiscountType;
import com.example.pos_system.entity.MarketingCampaign;
import com.example.pos_system.entity.MarketingCampaignType;
import com.example.pos_system.entity.SaleStatus;
import com.example.pos_system.repository.MarketingCampaignRepo;
import com.example.pos_system.repository.SaleRepo;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.List;

@Service
public class MarketingPricingService {
    private static final String AUTO_REASON_PREFIX = "[AUTO_CAMPAIGN] ";
    private final MarketingCampaignRepo marketingCampaignRepo;
    private final SaleRepo saleRepo;

    /**
     * Executes the MarketingPricingService operation.
     * <p>Return value: A fully initialized MarketingPricingService instance.</p>
     *
     * @param marketingCampaignRepo Parameter of type {@code MarketingCampaignRepo} used by this operation.
     * @param saleRepo Parameter of type {@code SaleRepo} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public MarketingPricingService(MarketingCampaignRepo marketingCampaignRepo, SaleRepo saleRepo) {
        this.marketingCampaignRepo = marketingCampaignRepo;
        this.saleRepo = saleRepo;
    }

    /**
     * Executes the applyBestCampaign operation.
     *
     * @param cart Parameter of type {@code Cart} used by this operation.
     * @param customer Parameter of type {@code Customer} used by this operation.
     * @return {@code AppliedCampaign} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public AppliedCampaign applyBestCampaign(Cart cart, Customer customer) {
        if (cart == null) return AppliedCampaign.none();

        if (cart.getItems().isEmpty()) {
            clearAutoCampaignDiscount(cart);
            return AppliedCampaign.none();
        }

        if (hasManualDiscount(cart)) {
            return AppliedCampaign.none();
        }

        BigDecimal subtotal = safeMoney(cart.getSubtotal());
        if (subtotal.compareTo(BigDecimal.ZERO) <= 0) {
            clearAutoCampaignDiscount(cart);
            return AppliedCampaign.none();
        }

        LocalDateTime now = LocalDateTime.now();
        List<MarketingCampaign> activeCampaigns =
                marketingCampaignRepo.findByActiveTrueAndStartsAtLessThanEqualAndEndsAtGreaterThanEqualOrderByCreatedAtDesc(now, now);
        if (activeCampaigns.isEmpty()) {
            clearAutoCampaignDiscount(cart);
            return AppliedCampaign.none();
        }

        boolean hasPriorNonVoidSale = hasPriorNonVoidSale(customer);
        MarketingCampaign winner = null;
        BigDecimal winnerDiscount = BigDecimal.ZERO;

        for (MarketingCampaign campaign : activeCampaigns) {
            if (!isEligible(campaign, subtotal, customer, hasPriorNonVoidSale)) {
                continue;
            }
            BigDecimal candidate = calculateDiscount(campaign, subtotal);
            if (candidate.compareTo(winnerDiscount) > 0) {
                winner = campaign;
                winnerDiscount = candidate;
            }
        }

        if (winner == null || winnerDiscount.compareTo(BigDecimal.ZERO) <= 0) {
            clearAutoCampaignDiscount(cart);
            return AppliedCampaign.none();
        }

        BigDecimal appliedAmount = winnerDiscount.min(subtotal).max(BigDecimal.ZERO).setScale(2, RoundingMode.HALF_UP);
        cart.setDiscountType(DiscountType.AMOUNT);
        cart.setDiscountValue(appliedAmount);
        cart.setDiscountReason(buildAutoReason(winner));
        cart.setManualDiscountOverride(false);

        return new AppliedCampaign(winner.getId(), winner.getTitle(), winner.getType(), appliedAmount);
    }

    /**
     * Executes the isAutoCampaignReason operation.
     *
     * @param reason Parameter of type {@code String} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public boolean isAutoCampaignReason(String reason) {
        return reason != null && reason.startsWith(AUTO_REASON_PREFIX);
    }

    /**
     * Executes the hasManualDiscount operation.
     *
     * @param cart Parameter of type {@code Cart} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean hasManualDiscount(Cart cart) {
        if (cart.isManualDiscountOverride()) {
            return true;
        }
        BigDecimal currentDiscount = safeMoney(cart.getDiscount());
        if (currentDiscount.compareTo(BigDecimal.ZERO) <= 0) {
            return false;
        }
        return !isAutoCampaignReason(cart.getDiscountReason());
    }

    /**
     * Executes the clearAutoCampaignDiscount operation.
     *
     * @param cart Parameter of type {@code Cart} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void clearAutoCampaignDiscount(Cart cart) {
        if (cart == null || cart.isManualDiscountOverride()) {
            return;
        }
        if (!isAutoCampaignReason(cart.getDiscountReason())) {
            return;
        }
        cart.setDiscountType(DiscountType.AMOUNT);
        cart.setDiscountValue(BigDecimal.ZERO);
        cart.setDiscountReason(null);
    }

    /**
     * Executes the hasPriorNonVoidSale operation.
     *
     * @param customer Parameter of type {@code Customer} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean hasPriorNonVoidSale(Customer customer) {
        if (customer == null || customer.getId() == null) return false;
        return saleRepo.existsByCustomer_IdAndStatusNot(customer.getId(), SaleStatus.VOID);
    }

    /**
     * Executes the isEligible operation.
     *
     * @param campaign Parameter of type {@code MarketingCampaign} used by this operation.
     * @param subtotal Parameter of type {@code BigDecimal} used by this operation.
     * @param customer Parameter of type {@code Customer} used by this operation.
     * @param hasPriorNonVoidSale Parameter of type {@code boolean} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean isEligible(MarketingCampaign campaign,
                               BigDecimal subtotal,
                               Customer customer,
                               boolean hasPriorNonVoidSale) {
        if (campaign == null) return false;
        if (campaign.getMinSpend() != null && subtotal.compareTo(campaign.getMinSpend()) < 0) {
            return false;
        }
        if (campaign.getType() == MarketingCampaignType.FIRST_ORDER_DISCOUNT) {
            if (customer == null || customer.getId() == null) {
                return false;
            }
            return !hasPriorNonVoidSale;
        }
        return true;
    }

    /**
     * Executes the calculateDiscount operation.
     *
     * @param campaign Parameter of type {@code MarketingCampaign} used by this operation.
     * @param subtotal Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal calculateDiscount(MarketingCampaign campaign, BigDecimal subtotal) {
        BigDecimal fixedAmount = safeMoney(campaign.getDiscountAmount());
        BigDecimal percentAmount = BigDecimal.ZERO;

        BigDecimal percent = safeMoney(campaign.getDiscountPercent());
        if (percent.compareTo(BigDecimal.ZERO) > 0) {
            BigDecimal clampedPercent = percent.min(new BigDecimal("100"));
            percentAmount = subtotal.multiply(clampedPercent)
                    .divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP);
        }

        BigDecimal result = fixedAmount.max(percentAmount);
        if (result.compareTo(subtotal) > 0) {
            result = subtotal;
        }
        return result.max(BigDecimal.ZERO);
    }

    /**
     * Executes the buildAutoReason operation.
     *
     * @param campaign Parameter of type {@code MarketingCampaign} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String buildAutoReason(MarketingCampaign campaign) {
        String title = campaign == null || campaign.getTitle() == null ? "Campaign" : campaign.getTitle();
        return AUTO_REASON_PREFIX + title;
    }

    /**
     * Executes the safeMoney operation.
     *
     * @param value Parameter of type {@code BigDecimal} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal safeMoney(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    public record AppliedCampaign(Long campaignId,
                                  String title,
                                  MarketingCampaignType type,
                                  BigDecimal discountAmount) {
        /**
         * Executes the none operation.
         *
         * @return {@code AppliedCampaign} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        public static AppliedCampaign none() {
            return new AppliedCampaign(null, null, null, BigDecimal.ZERO);
        }

        /**
         * Executes the applied operation.
         *
         * @return {@code boolean} Result produced by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        public boolean applied() {
            return campaignId != null && discountAmount != null && discountAmount.compareTo(BigDecimal.ZERO) > 0;
        }
    }
}
