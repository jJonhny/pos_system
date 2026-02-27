import { Navigate, Outlet, useLocation } from 'react-router-dom'
import { useAuth } from '../../features/auth/useAuth'
import { hasAnyRole } from '../../shared/auth/roles'
import { Spinner } from '../../shared/ui/Spinner'

export function ProtectedRoute({ allowedRoles = [] }) {
  const { isAuthenticated, auth, initializing } = useAuth()
  const location = useLocation()

  if (initializing) {
    return <Spinner label="Checking authentication..." />
  }

  if (!isAuthenticated) {
    return <Navigate to="/login" replace state={{ from: location }} />
  }

  if (!hasAnyRole(auth?.role, allowedRoles)) {
    return <Navigate to="/unauthorized" replace />
  }

  return <Outlet />
}
