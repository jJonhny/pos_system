package com.devcore.pos_system.api.v1.controller;

import com.devcore.pos_system.api.v1.dto.common.ApiEnvelope;
import com.devcore.pos_system.api.v1.dto.common.ApiPageData;
import com.devcore.pos_system.api.v1.dto.user.ApiUserDto;
import com.devcore.pos_system.api.v1.dto.user.UserCreateRequest;
import com.devcore.pos_system.api.v1.dto.user.UserPermissionsUpdateRequest;
import com.devcore.pos_system.api.v1.dto.user.UserRoleUpdateRequest;
import com.devcore.pos_system.api.v1.dto.user.UserStatusUpdateRequest;
import com.devcore.pos_system.api.v1.service.ApiUserService;
import com.devcore.pos_system.api.v1.support.ApiTrace;
import com.devcore.pos_system.entity.UserRole;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/users")
public class UserApiV1Controller {
    private static final int DEFAULT_PAGE_SIZE = 20;
    private static final int MAX_PAGE_SIZE = 200;

    private final ApiUserService apiUserService;

    public UserApiV1Controller(ApiUserService apiUserService) {
        this.apiUserService = apiUserService;
    }

    @GetMapping
    public ApiEnvelope<ApiPageData<ApiUserDto>> list(@RequestParam(required = false) String q,
                                                     @RequestParam(required = false) UserRole role,
                                                     @RequestParam(required = false) Boolean active,
                                                     @RequestParam(defaultValue = "0") int page,
                                                     @RequestParam(defaultValue = "20") int size,
                                                     @RequestParam(defaultValue = "username") String sort,
                                                     @RequestParam(defaultValue = "asc") String dir,
                                                     HttpServletRequest request) {
        int safePage = Math.max(0, page);
        int safeSize = normalizePageSize(size);
        Page<ApiUserDto> users = apiUserService.listUsers(
                q,
                role,
                active,
                PageRequest.of(safePage, safeSize, sortBy(sort, dir))
        );

        return ApiEnvelope.success(
                "USERS_LIST_OK",
                "Users fetched successfully.",
                ApiPageData.from(users),
                ApiTrace.resolve(request)
        );
    }

    @GetMapping("/{id}")
    public ApiEnvelope<ApiUserDto> getById(@PathVariable Long id, HttpServletRequest request) {
        return ApiEnvelope.success(
                "USER_FETCH_OK",
                "User fetched successfully.",
                apiUserService.getById(id),
                ApiTrace.resolve(request)
        );
    }

    @PostMapping
    public ResponseEntity<ApiEnvelope<ApiUserDto>> create(@Valid @RequestBody UserCreateRequest requestBody,
                                                          Authentication authentication,
                                                          HttpServletRequest request) {
        ApiUserDto created = apiUserService.create(requestBody, authentication);
        return ResponseEntity.status(HttpStatus.CREATED).body(
                ApiEnvelope.success(
                        "USER_CREATE_OK",
                        "User created successfully.",
                        created,
                        ApiTrace.resolve(request)
                )
        );
    }

    @PatchMapping("/{id}/role")
    public ApiEnvelope<ApiUserDto> updateRole(@PathVariable Long id,
                                              @Valid @RequestBody UserRoleUpdateRequest requestBody,
                                              Authentication authentication,
                                              HttpServletRequest request) {
        return ApiEnvelope.success(
                "USER_ROLE_UPDATE_OK",
                "User role updated successfully.",
                apiUserService.updateRole(id, requestBody.role(), authentication),
                ApiTrace.resolve(request)
        );
    }

    @PatchMapping("/{id}/status")
    public ApiEnvelope<ApiUserDto> updateStatus(@PathVariable Long id,
                                                @Valid @RequestBody UserStatusUpdateRequest requestBody,
                                                Authentication authentication,
                                                HttpServletRequest request) {
        return ApiEnvelope.success(
                "USER_STATUS_UPDATE_OK",
                "User status updated successfully.",
                apiUserService.updateStatus(id, Boolean.TRUE.equals(requestBody.active()), authentication),
                ApiTrace.resolve(request)
        );
    }

    @PutMapping("/{id}/permissions")
    public ApiEnvelope<ApiUserDto> updatePermissions(@PathVariable Long id,
                                                     @Valid @RequestBody UserPermissionsUpdateRequest requestBody,
                                                     Authentication authentication,
                                                     HttpServletRequest request) {
        return ApiEnvelope.success(
                "USER_PERMISSIONS_UPDATE_OK",
                "User permissions updated successfully.",
                apiUserService.updatePermissions(id, requestBody.permissions(), authentication),
                ApiTrace.resolve(request)
        );
    }

    private Sort sortBy(String sort, String dir) {
        String property = (sort == null || sort.isBlank()) ? "username" : sort.trim();
        Sort.Direction direction = "desc".equalsIgnoreCase(dir) ? Sort.Direction.DESC : Sort.Direction.ASC;
        return Sort.by(direction, property);
    }

    private int normalizePageSize(int requested) {
        if (requested <= 0) {
            return DEFAULT_PAGE_SIZE;
        }
        return Math.min(requested, MAX_PAGE_SIZE);
    }
}
