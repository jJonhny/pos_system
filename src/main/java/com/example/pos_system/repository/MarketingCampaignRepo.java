package com.example.pos_system.repository;

import com.example.pos_system.entity.MarketingCampaign;
import com.example.pos_system.entity.MarketingCampaignType;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;
import java.util.List;

public interface MarketingCampaignRepo extends JpaRepository<MarketingCampaign, Long> {
    List<MarketingCampaign> findTop100ByTypeOrderByCreatedAtDesc(MarketingCampaignType type);
    List<MarketingCampaign> findByActiveTrueAndStartsAtLessThanEqualAndEndsAtGreaterThanEqualOrderByCreatedAtDesc(
            LocalDateTime startsAt,
            LocalDateTime endsAt
    );
}
