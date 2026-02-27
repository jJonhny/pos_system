const AUTH_KEY = 'pos_admin_auth_v1'

export function loadAuthSnapshot() {
  const raw = localStorage.getItem(AUTH_KEY)
  if (!raw) return null
  try {
    return JSON.parse(raw)
  } catch (_) {
    localStorage.removeItem(AUTH_KEY)
    return null
  }
}

export function saveAuthSnapshot(snapshot) {
  localStorage.setItem(AUTH_KEY, JSON.stringify(snapshot))
}

export function clearAuthSnapshot() {
  localStorage.removeItem(AUTH_KEY)
}

export function getAccessToken() {
  return loadAuthSnapshot()?.accessToken || null
}
