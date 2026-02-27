package com.example.pos_system.service;

import com.example.pos_system.entity.AppUser;
import com.example.pos_system.entity.UserRole;
import com.example.pos_system.repository.AppUserRepo;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsPasswordService;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Service
public class AppUserDetailsService implements UserDetailsService, UserDetailsPasswordService {
    private final AppUserRepo appUserRepo;

    /**
     * Executes the AppUserDetailsService operation.
     * <p>Return value: A fully initialized AppUserDetailsService instance.</p>
     *
     * @param appUserRepo Parameter of type {@code AppUserRepo} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public AppUserDetailsService(AppUserRepo appUserRepo) {
        this.appUserRepo = appUserRepo;
    }

    /**
     * Executes the loadUserByUsername operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @return {@code UserDetails} Result produced by this operation.
     * @throws UsernameNotFoundException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the loadUserByUsername operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @return {@code UserDetails} Result produced by this operation.
     * @throws UsernameNotFoundException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the loadUserByUsername operation.
     *
     * @param username Parameter of type {@code String} used by this operation.
     * @return {@code UserDetails} Result produced by this operation.
     * @throws UsernameNotFoundException If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        AppUser user = appUserRepo.findByUsernameIgnoreCaseOrEmailIgnoreCase(username, username)
                .orElseThrow(() -> new UsernameNotFoundException("User not found"));
        return User.builder()
                .username(user.getUsername())
                .password(user.getPassword())
                .authorities(buildAuthorities(user))
                .disabled(Boolean.FALSE.equals(user.getActive()))
                .accountLocked(isCurrentlyLocked(user))
                .build();
    }

    /**
     * Executes the updatePassword operation.
     *
     * @param user Parameter of type {@code UserDetails} used by this operation.
     * @param newPassword Parameter of type {@code String} used by this operation.
     * @return {@code UserDetails} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updatePassword operation.
     *
     * @param user Parameter of type {@code UserDetails} used by this operation.
     * @param newPassword Parameter of type {@code String} used by this operation.
     * @return {@code UserDetails} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the updatePassword operation.
     *
     * @param user Parameter of type {@code UserDetails} used by this operation.
     * @param newPassword Parameter of type {@code String} used by this operation.
     * @return {@code UserDetails} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Override
    @Transactional
    public UserDetails updatePassword(UserDetails user, String newPassword) {
        AppUser appUser = appUserRepo.findByUsernameIgnoreCaseOrEmailIgnoreCase(user.getUsername(), user.getUsername())
                .orElseThrow(() -> new UsernameNotFoundException("User not found"));
        appUser.setPassword(newPassword);
        appUserRepo.save(appUser);
        return User.builder()
                .username(appUser.getUsername())
                .password(appUser.getPassword())
                .authorities(buildAuthorities(appUser))
                .disabled(Boolean.FALSE.equals(appUser.getActive()))
                .build();
    }

    /**
     * Executes the buildAuthorities operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @return {@code List<GrantedAuthority>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private List<GrantedAuthority> buildAuthorities(AppUser user) {
        List<GrantedAuthority> authorities = new ArrayList<>();
        authorities.add(new SimpleGrantedAuthority("ROLE_" + user.getRole().name()));

        if (user.getRole() != null) {
            if (user.getRole() == UserRole.SUPER_ADMIN) {
                authorities.add(new SimpleGrantedAuthority("ROLE_ADMIN"));
                authorities.add(new SimpleGrantedAuthority("ROLE_SUPER_ADMIN"));
            } else if (user.getRole() == UserRole.ADMIN) {
                authorities.add(new SimpleGrantedAuthority("ROLE_SUPER_ADMIN"));
            }

            if (user.getRole() == UserRole.BRANCH_MANAGER) {
                authorities.add(new SimpleGrantedAuthority("ROLE_MANAGER"));
                authorities.add(new SimpleGrantedAuthority("ROLE_BRANCH_MANAGER"));
            } else if (user.getRole() == UserRole.MANAGER) {
                authorities.add(new SimpleGrantedAuthority("ROLE_BRANCH_MANAGER"));
            }
        }

        if (user.getPermissions() != null) {
            user.getPermissions().forEach(permission ->
                    authorities.add(new SimpleGrantedAuthority("PERM_" + permission.name())));
        }
        return authorities;
    }

    /**
     * Executes the isCurrentlyLocked operation.
     *
     * @param user Parameter of type {@code AppUser} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean isCurrentlyLocked(AppUser user) {
        if (user == null || user.getLockedUntil() == null) {
            return false;
        }
        return user.getLockedUntil().isAfter(LocalDateTime.now());
    }
}
