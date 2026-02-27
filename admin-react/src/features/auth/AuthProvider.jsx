import { createContext, useContext, useEffect, useMemo, useState } from 'react'
import { me } from '../../shared/api/endpoints/authApi'
import {
  clearAuthSnapshot,
  loadAuthSnapshot,
  saveAuthSnapshot
} from '../../shared/auth/tokenStorage'
import { normalizeRole } from '../../shared/auth/roles'

const AuthContext = createContext(null)

export function AuthProvider({ children }) {
  const [auth, setAuth] = useState(() => loadAuthSnapshot())
  const [initializing, setInitializing] = useState(true)

  useEffect(() => {
    let active = true
    const bootstrap = async () => {
      const snapshot = loadAuthSnapshot()
      if (!snapshot?.accessToken) {
        if (active) setInitializing(false)
        return
      }
      try {
        const profile = await me()
        if (!active) return
        const merged = {
          ...snapshot,
          role: normalizeRole(profile.role || snapshot.role),
          permissions: profile.permissions || snapshot.permissions || [],
          userId: profile.userId || snapshot.userId,
          username: profile.username || snapshot.username,
          email: profile.email || snapshot.email
        }
        saveAuthSnapshot(merged)
        setAuth(merged)
      } catch (_) {
        clearAuthSnapshot()
        if (active) setAuth(null)
      } finally {
        if (active) setInitializing(false)
      }
    }
    bootstrap()
    return () => {
      active = false
    }
  }, [])

  const value = useMemo(() => {
    return {
      auth,
      initializing,
      isAuthenticated: Boolean(auth?.accessToken),
      async completeLogin(otpResult) {
        const snapshot = {
          accessToken: otpResult.accessToken,
          role: normalizeRole(otpResult.role),
          permissions: otpResult.permissions || []
        }
        try {
          const profile = await me()
          snapshot.userId = profile.userId
          snapshot.username = profile.username
          snapshot.email = profile.email
          snapshot.role = normalizeRole(profile.role || snapshot.role)
          snapshot.permissions = profile.permissions || snapshot.permissions
        } catch (_) {
          // Keep login if profile call fails; token is already valid.
        }
        saveAuthSnapshot(snapshot)
        setAuth(snapshot)
      },
      logout() {
        clearAuthSnapshot()
        setAuth(null)
      }
    }
  }, [auth, initializing])

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>
}

export function useAuth() {
  const ctx = useContext(AuthContext)
  if (!ctx) {
    throw new Error('useAuth must be used inside AuthProvider')
  }
  return ctx
}
