package com.devcore.pos_system.api.v1.dto.common;

import org.springframework.data.domain.Page;

import java.util.List;

public record ApiPageData<T>(
        List<T> items,
        int page,
        int size,
        long totalElements,
        int totalPages,
        boolean hasNext,
        boolean hasPrevious
) {
    public static <T> ApiPageData<T> from(Page<T> page) {
        return new ApiPageData<>(
                page.getContent(),
                page.getNumber(),
                page.getSize(),
                page.getTotalElements(),
                page.getTotalPages(),
                page.hasNext(),
                page.hasPrevious()
        );
    }
}
