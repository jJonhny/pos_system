export const ROLES = {
  ADMIN: 'ADMIN',
  MANAGER: 'MANAGER',
  CASHIER: 'CASHIER',
  SUPER_ADMIN: 'SUPER_ADMIN',
  BRANCH_MANAGER: 'BRANCH_MANAGER',
  INVENTORY_STAFF: 'INVENTORY_STAFF'
}

export const ADMIN_ROLES = [ROLES.ADMIN, ROLES.SUPER_ADMIN]
export const MANAGEMENT_ROLES = [
  ROLES.ADMIN,
  ROLES.SUPER_ADMIN,
  ROLES.MANAGER,
  ROLES.BRANCH_MANAGER
]

export function normalizeRole(role) {
  if (!role) return null
  if (role.startsWith('ROLE_')) return role.slice(5)
  return role
}

export function hasAnyRole(currentRole, allowedRoles) {
  if (!allowedRoles || allowedRoles.length === 0) return true
  const normalized = normalizeRole(currentRole)
  return allowedRoles.some((r) => normalizeRole(r) === normalized)
}
