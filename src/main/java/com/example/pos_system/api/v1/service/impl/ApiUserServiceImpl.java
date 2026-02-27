package com.example.pos_system.api.v1.service.impl;

import com.example.pos_system.api.v1.dto.user.ApiUserDto;
import com.example.pos_system.api.v1.dto.user.UserCreateRequest;
import com.example.pos_system.api.v1.exception.ApiBadRequestException;
import com.example.pos_system.api.v1.exception.ApiNotFoundException;
import com.example.pos_system.api.v1.mapper.ApiV1Mapper;
import com.example.pos_system.api.v1.service.ApiUserService;
import com.example.pos_system.entity.AppUser;
import com.example.pos_system.entity.Permission;
import com.example.pos_system.entity.UserRole;
import com.example.pos_system.repository.AppUserRepo;
import com.example.pos_system.service.UserAdminService;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

@Service
@Transactional
public class ApiUserServiceImpl implements ApiUserService {
    private final AppUserRepo appUserRepo;
    private final UserAdminService userAdminService;
    private final ApiV1Mapper mapper;

    public ApiUserServiceImpl(AppUserRepo appUserRepo,
                              UserAdminService userAdminService,
                              ApiV1Mapper mapper) {
        this.appUserRepo = appUserRepo;
        this.userAdminService = userAdminService;
        this.mapper = mapper;
    }

    @Override
    @Transactional(readOnly = true)
    public Page<ApiUserDto> listUsers(String q, UserRole role, Boolean active, Pageable pageable) {
        Specification<AppUser> spec = buildSpecification(q, role, active);
        return appUserRepo.findAll(spec, pageable).map(mapper::toUserDto);
    }

    @Override
    @Transactional(readOnly = true)
    public ApiUserDto getById(Long id) {
        return mapper.toUserDto(requireUser(id));
    }

    @Override
    public ApiUserDto create(UserCreateRequest request, Authentication actor) {
        String username = normalize(request.username());
        if (username == null) {
            throw new ApiBadRequestException("username is required.");
        }
        if (appUserRepo.existsByUsernameIgnoreCase(username)) {
            throw new ApiBadRequestException("username already exists.");
        }

        String email = normalizeEmail(request.email());
        if (email != null && appUserRepo.existsByEmailIgnoreCase(email)) {
            throw new ApiBadRequestException("email already exists.");
        }

        Set<Permission> permissions = request.permissions() == null
                ? Set.of()
                : new HashSet<>(request.permissions());

        AppUser created = userAdminService.createUser(
                username,
                request.password(),
                request.role(),
                request.active() == null || request.active(),
                request.mustResetPassword() == null || request.mustResetPassword(),
                request.mfaRequired() != null && request.mfaRequired(),
                permissions,
                actor
        );

        if (email != null) {
            created.setEmail(email);
            created = appUserRepo.save(created);
        }

        return mapper.toUserDto(created);
    }

    @Override
    public ApiUserDto updateRole(Long id, UserRole role, Authentication actor) {
        AppUser user = requireUser(id);
        if (user.getRole() == UserRole.ADMIN && role != UserRole.ADMIN && appUserRepo.countByRole(UserRole.ADMIN) <= 1) {
            throw new ApiBadRequestException("at least one ADMIN account is required.");
        }
        userAdminService.updateRole(user, role, actor, "Role updated via API.");
        return mapper.toUserDto(requireUser(id));
    }

    @Override
    public ApiUserDto updateStatus(Long id, boolean active, Authentication actor) {
        AppUser user = requireUser(id);
        if (user.getRole() == UserRole.ADMIN && !active && appUserRepo.countByRole(UserRole.ADMIN) <= 1) {
            throw new ApiBadRequestException("cannot deactivate the last ADMIN account.");
        }
        userAdminService.updateStatus(user, active, actor, "Status updated via API.");
        return mapper.toUserDto(requireUser(id));
    }

    @Override
    public ApiUserDto updatePermissions(Long id, Set<Permission> permissions, Authentication actor) {
        AppUser user = requireUser(id);
        Set<Permission> updated = permissions == null ? Set.of() : new HashSet<>(permissions);
        userAdminService.updatePermissions(user, updated, actor, "Permissions updated via API.");
        return mapper.toUserDto(requireUser(id));
    }

    private AppUser requireUser(Long id) {
        if (id == null) {
            throw new ApiBadRequestException("user id is required.");
        }
        return appUserRepo.findById(id)
                .orElseThrow(() -> new ApiNotFoundException("user not found."));
    }

    private String normalize(String value) {
        if (value == null) {
            return null;
        }
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private String normalizeEmail(String value) {
        String normalized = normalize(value);
        return normalized == null ? null : normalized.toLowerCase(Locale.ROOT);
    }

    private Specification<AppUser> buildSpecification(String q, UserRole role, Boolean active) {
        return (root, query, cb) -> {
            var predicates = new ArrayList<Predicate>();
            String text = normalize(q);
            if (text != null) {
                String like = "%" + text.toLowerCase(Locale.ROOT) + "%";
                predicates.add(
                        cb.or(
                                cb.like(cb.lower(root.get("username")), like),
                                cb.like(cb.lower(cb.coalesce(root.get("email"), "")), like)
                        )
                );
            }
            if (role != null) {
                predicates.add(cb.equal(root.get("role"), role));
            }
            if (active != null) {
                predicates.add(cb.equal(root.get("active"), active));
            }
            return cb.and(predicates.toArray(Predicate[]::new));
        };
    }
}
