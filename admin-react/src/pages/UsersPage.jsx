import { useEffect, useState } from 'react'
import {
  createUser,
  listUsers,
  updateUserPermissions,
  updateUserRole,
  updateUserStatus
} from '../shared/api/endpoints/usersApi'
import { ROLES } from '../shared/auth/roles'
import { Spinner } from '../shared/ui/Spinner'

const ROLE_OPTIONS = Object.values(ROLES)

export function UsersPage() {
  const [query, setQuery] = useState('')
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState('')
  const [flash, setFlash] = useState('')
  const [pageData, setPageData] = useState({ items: [], totalElements: 0 })
  const [rowRoleDraft, setRowRoleDraft] = useState({})
  const [form, setForm] = useState({
    username: '',
    email: '',
    password: '',
    role: ROLES.CASHIER
  })

  const load = async () => {
    setLoading(true)
    setError('')
    try {
      const data = await listUsers({
        q: query || undefined,
        page: 0,
        size: 30,
        sort: 'username',
        dir: 'asc'
      })
      setPageData(data || { items: [], totalElements: 0 })
      setRowRoleDraft(
        Object.fromEntries((data?.items || []).map((u) => [u.id, u.role || ROLES.CASHIER]))
      )
    } catch (err) {
      setError(err.message || 'Failed to load users.')
    } finally {
      setLoading(false)
    }
  }

  useEffect(() => {
    load()
  }, [])

  const onCreate = async (e) => {
    e.preventDefault()
    setError('')
    setFlash('')
    try {
      await createUser({
        username: form.username.trim(),
        email: form.email.trim() || null,
        password: form.password,
        role: form.role
      })
      setFlash('User created.')
      setForm({ username: '', email: '', password: '', role: ROLES.CASHIER })
      await load()
    } catch (err) {
      setError(err.message || 'Failed to create user.')
    }
  }

  const onUpdateRole = async (userId) => {
    setError('')
    setFlash('')
    try {
      await updateUserRole(userId, rowRoleDraft[userId])
      setFlash('Role updated.')
      await load()
    } catch (err) {
      setError(err.message || 'Failed to update role.')
    }
  }

  const onToggleStatus = async (user) => {
    setError('')
    setFlash('')
    try {
      await updateUserStatus(user.id, !user.active)
      setFlash('Status updated.')
      await load()
    } catch (err) {
      setError(err.message || 'Failed to update status.')
    }
  }

  const onClearPermissions = async (userId) => {
    setError('')
    setFlash('')
    try {
      await updateUserPermissions(userId, [])
      setFlash('Permissions cleared.')
      await load()
    } catch (err) {
      setError(err.message || 'Failed to update permissions.')
    }
  }

  if (loading) return <Spinner label="Loading users..." />

  return (
    <div className="page">
      <div className="page-head">
        <h2>User Management</h2>
        <p>Create and manage user accounts, roles, and permissions</p>
      </div>

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>🔍 Search Users</h3>
        <form className="inline-form" onSubmit={(e) => e.preventDefault()}>
          <input
            placeholder="Search by username or email..."
            value={query}
            onChange={(e) => setQuery(e.target.value)}
            style={{ flex: 1, minWidth: '200px' }}
          />
          <button className="btn btn-primary" onClick={load}>
            Search
          </button>
        </form>
      </div>

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>➕ Create New User</h3>
        <form className="stack-form" onSubmit={onCreate}>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(240px, 1fr))', gap: '16px' }}>
            <label>
              Username <span style={{ color: 'var(--bad)' }}>*</span>
              <input
                required
                placeholder="e.g., john.doe (not email)"
                value={form.username}
                onChange={(e) => setForm((s) => ({ ...s, username: e.target.value }))}
                title="Enter a username (e.g., john.doe), not an email address"
              />
            </label>
            <label>
              Email
              <input
                type="email"
                placeholder="e.g., john@example.com"
                value={form.email}
                onChange={(e) => setForm((s) => ({ ...s, email: e.target.value }))}
              />
            </label>
            <label>
              Password <span style={{ color: 'var(--bad)' }}>*</span>
              <input
                type="password"
                required
                minLength={8}
                placeholder="Min 8 characters"
                value={form.password}
                onChange={(e) => setForm((s) => ({ ...s, password: e.target.value }))}
              />
            </label>
            <label>
              Role <span style={{ color: 'var(--bad)' }}>*</span>
              <select
                value={form.role}
                onChange={(e) => setForm((s) => ({ ...s, role: e.target.value }))}
              >
                {ROLE_OPTIONS.map((role) => (
                  <option key={role} value={role}>
                    {role}
                  </option>
                ))}
              </select>
            </label>
          </div>
          <div style={{ display: 'flex', gap: '8px', marginTop: '8px' }}>
            <button className="btn btn-primary" type="submit">
              ✓ Create User
            </button>
            <button
              className="btn btn-secondary"
              type="button"
              onClick={() => setForm({ username: '', email: '', password: '', role: ROLES.CASHIER })}
            >
              Clear
            </button>
          </div>
        </form>
      </div>

      {flash && (
        <div className="status-ok" style={{ margin: '16px 0' }}>
          <strong>✓</strong> {flash}
        </div>
      )}
      {error && (
        <div className="status-error" style={{ margin: '16px 0' }}>
          <strong>⚠️</strong> {error}
        </div>
      )}

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>
          👥 Users <span style={{ fontSize: '0.85rem', color: 'var(--muted)', fontWeight: 500 }}>({pageData.totalElements || 0})</span>
        </h3>
        {pageData.items && pageData.items.length === 0 ? (
          <div style={{ padding: '32px', textAlign: 'center', color: 'var(--muted)' }}>
            <p style={{ margin: 0, fontSize: '1rem', fontWeight: 500 }}>No users found</p>
            <p style={{ margin: '8px 0 0', fontSize: '0.9rem' }}>Create your first user to get started</p>
          </div>
        ) : (
          <div className="table-wrap">
            <table>
              <thead>
                <tr>
                  <th>ID</th>
                  <th>Username</th>
                  <th>Email</th>
                  <th>Role</th>
                  <th>Status</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {(pageData.items || []).map((user) => (
                  <tr key={user.id}>
                    <td style={{ fontWeight: 600, color: 'var(--brand)' }}>{user.id}</td>
                    <td style={{ fontWeight: 600 }}>{user.username}</td>
                    <td>{user.email || '—'}</td>
                    <td>
                      <div style={{ display: 'flex', gap: '8px', alignItems: 'center' }}>
                        <select
                          value={rowRoleDraft[user.id] || user.role}
                          onChange={(e) =>
                            setRowRoleDraft((s) => ({ ...s, [user.id]: e.target.value }))
                          }
                          style={{ minWidth: '120px' }}
                        >
                          {ROLE_OPTIONS.map((role) => (
                            <option key={role} value={role}>
                              {role}
                            </option>
                          ))}
                        </select>
                        <button
                          className="btn btn-secondary"
                          onClick={() => onUpdateRole(user.id)}
                          style={{ fontSize: '0.85rem', padding: '8px 12px' }}
                        >
                          Save
                        </button>
                      </div>
                    </td>
                    <td>
                      <span
                        style={{
                          display: 'inline-block',
                          padding: '4px 10px',
                          borderRadius: '6px',
                          fontSize: '0.85rem',
                          fontWeight: 600,
                          backgroundColor: user.active ? 'var(--good-light)' : 'var(--bad-light)',
                          color: user.active ? 'var(--good)' : 'var(--bad)'
                        }}
                      >
                        {user.active ? '✓ Active' : '✕ Inactive'}
                      </span>
                    </td>
                    <td>
                      <div style={{ display: 'flex', gap: '8px', flexWrap: 'wrap' }}>
                        <button
                          className="btn btn-secondary"
                          onClick={() => onToggleStatus(user)}
                          style={{ fontSize: '0.85rem', padding: '8px 12px' }}
                        >
                          {user.active ? 'Deactivate' : 'Activate'}
                        </button>
                        <button
                          className="btn btn-secondary"
                          onClick={() => onClearPermissions(user.id)}
                          style={{ fontSize: '0.85rem', padding: '8px 12px' }}
                        >
                          Clear Perms
                        </button>
                      </div>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>
    </div>
  )
}
