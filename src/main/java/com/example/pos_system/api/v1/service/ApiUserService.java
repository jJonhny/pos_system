package com.example.pos_system.api.v1.service;

import com.example.pos_system.api.v1.dto.user.ApiUserDto;
import com.example.pos_system.api.v1.dto.user.UserCreateRequest;
import com.example.pos_system.entity.Permission;
import com.example.pos_system.entity.UserRole;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.Authentication;

import java.util.Set;

public interface ApiUserService {
    Page<ApiUserDto> listUsers(String q, UserRole role, Boolean active, Pageable pageable);

    ApiUserDto getById(Long id);

    ApiUserDto create(UserCreateRequest request, Authentication actor);

    ApiUserDto updateRole(Long id, UserRole role, Authentication actor);

    ApiUserDto updateStatus(Long id, boolean active, Authentication actor);

    ApiUserDto updatePermissions(Long id, Set<Permission> permissions, Authentication actor);
}
