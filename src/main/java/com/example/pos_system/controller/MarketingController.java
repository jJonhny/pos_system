package com.example.pos_system.controller;

import com.example.pos_system.entity.MarketingCampaign;
import com.example.pos_system.entity.MarketingCampaignType;
import com.example.pos_system.repository.MarketingCampaignRepo;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Locale;

@Controller
@RequestMapping("/marketing")
public class MarketingController {
    private static final String DATE_TIME_PATTERN = "yyyy-MM-dd'T'HH:mm";
    private final MarketingCampaignRepo marketingCampaignRepo;

    public MarketingController(MarketingCampaignRepo marketingCampaignRepo) {
        this.marketingCampaignRepo = marketingCampaignRepo;
    }

    @GetMapping
    public String index(@RequestParam(required = false) String tab, Model model) {
        model.addAttribute("activeTab", normalizeTab(tab));
        model.addAttribute("now", LocalDateTime.now());
        model.addAttribute("discountEvents",
                marketingCampaignRepo.findTop100ByTypeOrderByCreatedAtDesc(MarketingCampaignType.DISCOUNT_EVENT));
        model.addAttribute("seckillEvents",
                marketingCampaignRepo.findTop100ByTypeOrderByCreatedAtDesc(MarketingCampaignType.SECKILL));
        model.addAttribute("couponEvents",
                marketingCampaignRepo.findTop100ByTypeOrderByCreatedAtDesc(MarketingCampaignType.COUPON));
        model.addAttribute("fullReductionEvents",
                marketingCampaignRepo.findTop100ByTypeOrderByCreatedAtDesc(MarketingCampaignType.FULL_REDUCTION));
        model.addAttribute("firstOrderEvents",
                marketingCampaignRepo.findTop100ByTypeOrderByCreatedAtDesc(MarketingCampaignType.FIRST_ORDER_DISCOUNT));
        return "marketing/index";
    }

    @PostMapping("/discounts")
    public String createDiscount(@RequestParam String title,
                                 @RequestParam(required = false) String description,
                                 @RequestParam(required = false) BigDecimal discountPercent,
                                 @RequestParam(required = false) BigDecimal discountAmount,
                                 @RequestParam(required = false) BigDecimal minSpend,
                                 @RequestParam @DateTimeFormat(pattern = DATE_TIME_PATTERN) LocalDateTime startsAt,
                                 @RequestParam @DateTimeFormat(pattern = DATE_TIME_PATTERN) LocalDateTime endsAt,
                                 RedirectAttributes redirectAttributes) {
        String baseValidation = validateBase(title, startsAt, endsAt);
        if (baseValidation != null) {
            return fail(redirectAttributes, "discount-add", baseValidation);
        }
        if (!isPositive(discountPercent) && !isPositive(discountAmount)) {
            return fail(redirectAttributes, "discount-add", "Provide a positive discount percent or amount.");
        }

        MarketingCampaign campaign = newCampaign(MarketingCampaignType.DISCOUNT_EVENT, title, description, startsAt, endsAt);
        campaign.setDiscountPercent(normalizePositiveDecimal(discountPercent));
        campaign.setDiscountAmount(normalizePositiveDecimal(discountAmount));
        campaign.setMinSpend(normalizePositiveDecimal(minSpend));
        marketingCampaignRepo.save(campaign);
        return ok(redirectAttributes, "discount-list", "Discount event created.");
    }

    @PostMapping("/seckill")
    public String createSeckill(@RequestParam String title,
                                @RequestParam(required = false) String description,
                                @RequestParam(required = false) BigDecimal discountPercent,
                                @RequestParam(required = false) BigDecimal discountAmount,
                                @RequestParam(required = false) BigDecimal minSpend,
                                @RequestParam @DateTimeFormat(pattern = DATE_TIME_PATTERN) LocalDateTime startsAt,
                                @RequestParam @DateTimeFormat(pattern = DATE_TIME_PATTERN) LocalDateTime endsAt,
                                RedirectAttributes redirectAttributes) {
        String baseValidation = validateBase(title, startsAt, endsAt);
        if (baseValidation != null) {
            return fail(redirectAttributes, "seckill-add", baseValidation);
        }
        if (!isPositive(discountPercent) && !isPositive(discountAmount)) {
            return fail(redirectAttributes, "seckill-add", "Provide a positive discount percent or amount.");
        }

        MarketingCampaign campaign = newCampaign(MarketingCampaignType.SECKILL, title, description, startsAt, endsAt);
        campaign.setDiscountPercent(normalizePositiveDecimal(discountPercent));
        campaign.setDiscountAmount(normalizePositiveDecimal(discountAmount));
        campaign.setMinSpend(normalizePositiveDecimal(minSpend));
        marketingCampaignRepo.save(campaign);
        return ok(redirectAttributes, "seckill-list", "Seckill activity created.");
    }

    @PostMapping("/coupons")
    public String createCoupon(@RequestParam String title,
                               @RequestParam String code,
                               @RequestParam(required = false) String description,
                               @RequestParam(required = false) BigDecimal discountPercent,
                               @RequestParam(required = false) BigDecimal discountAmount,
                               @RequestParam(required = false) BigDecimal minSpend,
                               @RequestParam(required = false) Integer couponStock,
                               @RequestParam(required = false) Integer perUserLimit,
                               @RequestParam @DateTimeFormat(pattern = DATE_TIME_PATTERN) LocalDateTime startsAt,
                               @RequestParam @DateTimeFormat(pattern = DATE_TIME_PATTERN) LocalDateTime endsAt,
                               RedirectAttributes redirectAttributes) {
        String baseValidation = validateBase(title, startsAt, endsAt);
        if (baseValidation != null) {
            return fail(redirectAttributes, "coupon-add", baseValidation);
        }
        if (clean(code) == null) {
            return fail(redirectAttributes, "coupon-add", "Coupon code is required.");
        }
        if (!isPositive(discountPercent) && !isPositive(discountAmount)) {
            return fail(redirectAttributes, "coupon-add", "Provide a positive discount percent or amount.");
        }

        MarketingCampaign campaign = newCampaign(MarketingCampaignType.COUPON, title, description, startsAt, endsAt);
        campaign.setCode(clean(code).toUpperCase(Locale.ROOT));
        campaign.setDiscountPercent(normalizePositiveDecimal(discountPercent));
        campaign.setDiscountAmount(normalizePositiveDecimal(discountAmount));
        campaign.setMinSpend(normalizePositiveDecimal(minSpend));
        campaign.setCouponStock(normalizePositiveInt(couponStock));
        campaign.setPerUserLimit(normalizePositiveInt(perUserLimit));
        marketingCampaignRepo.save(campaign);
        return ok(redirectAttributes, "coupon-list", "Coupon created.");
    }

    @PostMapping("/full-reductions")
    public String createFullReduction(@RequestParam String title,
                                      @RequestParam(required = false) String description,
                                      @RequestParam BigDecimal minSpend,
                                      @RequestParam BigDecimal discountAmount,
                                      @RequestParam @DateTimeFormat(pattern = DATE_TIME_PATTERN) LocalDateTime startsAt,
                                      @RequestParam @DateTimeFormat(pattern = DATE_TIME_PATTERN) LocalDateTime endsAt,
                                      RedirectAttributes redirectAttributes) {
        String baseValidation = validateBase(title, startsAt, endsAt);
        if (baseValidation != null) {
            return fail(redirectAttributes, "full-reduction-add", baseValidation);
        }
        if (!isPositive(minSpend) || !isPositive(discountAmount)) {
            return fail(redirectAttributes, "full-reduction-add", "Provide positive min spend and reduction values.");
        }

        MarketingCampaign campaign = newCampaign(MarketingCampaignType.FULL_REDUCTION, title, description, startsAt, endsAt);
        campaign.setMinSpend(normalizePositiveDecimal(minSpend));
        campaign.setDiscountAmount(normalizePositiveDecimal(discountAmount));
        marketingCampaignRepo.save(campaign);
        return ok(redirectAttributes, "full-reduction-list", "Full reduction event created.");
    }

    @PostMapping("/first-order-discounts")
    public String createFirstOrderDiscount(@RequestParam String title,
                                           @RequestParam(required = false) String description,
                                           @RequestParam(required = false) BigDecimal discountPercent,
                                           @RequestParam(required = false) BigDecimal discountAmount,
                                           @RequestParam(required = false) BigDecimal minSpend,
                                           @RequestParam @DateTimeFormat(pattern = DATE_TIME_PATTERN) LocalDateTime startsAt,
                                           @RequestParam @DateTimeFormat(pattern = DATE_TIME_PATTERN) LocalDateTime endsAt,
                                           RedirectAttributes redirectAttributes) {
        String baseValidation = validateBase(title, startsAt, endsAt);
        if (baseValidation != null) {
            return fail(redirectAttributes, "first-order-add", baseValidation);
        }
        if (!isPositive(discountPercent) && !isPositive(discountAmount)) {
            return fail(redirectAttributes, "first-order-add", "Provide a positive discount percent or amount.");
        }

        MarketingCampaign campaign = newCampaign(MarketingCampaignType.FIRST_ORDER_DISCOUNT, title, description, startsAt, endsAt);
        campaign.setDiscountPercent(normalizePositiveDecimal(discountPercent));
        campaign.setDiscountAmount(normalizePositiveDecimal(discountAmount));
        campaign.setMinSpend(normalizePositiveDecimal(minSpend));
        marketingCampaignRepo.save(campaign);
        return ok(redirectAttributes, "first-order-list", "First order discount created.");
    }

    @PostMapping("/{id}/toggle-active")
    public String toggleActive(@PathVariable Long id,
                               @RequestParam(required = false) String tab,
                               RedirectAttributes redirectAttributes) {
        MarketingCampaign campaign = marketingCampaignRepo.findById(id).orElse(null);
        if (campaign == null) {
            return fail(redirectAttributes, normalizeTab(tab), "Campaign not found.");
        }
        campaign.setActive(!Boolean.TRUE.equals(campaign.getActive()));
        marketingCampaignRepo.save(campaign);
        return ok(redirectAttributes, normalizeTab(tab), "Campaign status updated.");
    }

    @PostMapping("/{id}/delete")
    public String delete(@PathVariable Long id,
                         @RequestParam(required = false) String tab,
                         RedirectAttributes redirectAttributes) {
        if (!marketingCampaignRepo.existsById(id)) {
            return fail(redirectAttributes, normalizeTab(tab), "Campaign not found.");
        }
        marketingCampaignRepo.deleteById(id);
        return ok(redirectAttributes, normalizeTab(tab), "Campaign deleted.");
    }

    private MarketingCampaign newCampaign(MarketingCampaignType type,
                                          String title,
                                          String description,
                                          LocalDateTime startsAt,
                                          LocalDateTime endsAt) {
        MarketingCampaign campaign = new MarketingCampaign();
        campaign.setType(type);
        campaign.setTitle(clean(title));
        campaign.setDescription(clean(description));
        campaign.setStartsAt(startsAt);
        campaign.setEndsAt(endsAt);
        campaign.setActive(true);
        return campaign;
    }

    private String validateBase(String title, LocalDateTime startsAt, LocalDateTime endsAt) {
        if (clean(title) == null) {
            return "Title is required.";
        }
        if (startsAt == null || endsAt == null) {
            return "Start and end time are required.";
        }
        if (!endsAt.isAfter(startsAt)) {
            return "End time must be after start time.";
        }
        return null;
    }

    private String clean(String value) {
        if (value == null) {
            return null;
        }
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private boolean isPositive(BigDecimal value) {
        return value != null && value.compareTo(BigDecimal.ZERO) > 0;
    }

    private BigDecimal normalizePositiveDecimal(BigDecimal value) {
        return isPositive(value) ? value : null;
    }

    private Integer normalizePositiveInt(Integer value) {
        return value != null && value > 0 ? value : null;
    }

    private String ok(RedirectAttributes redirectAttributes, String tab, String message) {
        redirectAttributes.addFlashAttribute("success", message);
        return "redirect:/marketing?tab=" + normalizeTab(tab);
    }

    private String fail(RedirectAttributes redirectAttributes, String tab, String message) {
        redirectAttributes.addFlashAttribute("error", message);
        return "redirect:/marketing?tab=" + normalizeTab(tab);
    }

    private String normalizeTab(String tab) {
        if (tab == null || tab.isBlank()) {
            return "discount-add";
        }
        return tab;
    }
}
