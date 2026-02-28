package com.devcore.pos_system.repository;

import com.devcore.pos_system.entity.MarketingCampaign;
import com.devcore.pos_system.entity.MarketingCampaignType;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;
import java.util.List;

public interface MarketingCampaignRepo extends JpaRepository<MarketingCampaign, Long> {
    /**
     * Executes the findTop100ByTypeOrderByCreatedAtDesc operation.
     *
     * @param type Parameter of type {@code MarketingCampaignType} used by this operation.
     * @return {@code List<MarketingCampaign>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<MarketingCampaign> findTop100ByTypeOrderByCreatedAtDesc(MarketingCampaignType type);
    /**
     * Executes the findByActiveTrueAndStartsAtLessThanEqualAndEndsAtGreaterThanEqualOrderByCreatedAtDesc operation.
     *
     * @param startsAt Parameter of type {@code LocalDateTime} used by this operation.
     * @param endsAt Parameter of type {@code LocalDateTime} used by this operation.
     * @return {@code List<MarketingCampaign>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    List<MarketingCampaign> findByActiveTrueAndStartsAtLessThanEqualAndEndsAtGreaterThanEqualOrderByCreatedAtDesc(
            LocalDateTime startsAt,
            LocalDateTime endsAt
    );
}
