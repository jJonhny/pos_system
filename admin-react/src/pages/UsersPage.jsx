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
        <p>Backed by `GET/POST/PATCH/PUT /api/v1/users/*`</p>
      </div>

      <div className="panel">
        <form className="inline-form" onSubmit={(e) => e.preventDefault()}>
          <input
            placeholder="Search username/email"
            value={query}
            onChange={(e) => setQuery(e.target.value)}
          />
          <button className="btn btn-outline" onClick={load}>
            Search
          </button>
        </form>
      </div>

      <div className="panel">
        <h3>Create User</h3>
        <form className="stack-form" onSubmit={onCreate}>
          <label>
            Username
            <input
              required
              value={form.username}
              onChange={(e) => setForm((s) => ({ ...s, username: e.target.value }))}
            />
          </label>
          <label>
            Email
            <input
              type="email"
              value={form.email}
              onChange={(e) => setForm((s) => ({ ...s, email: e.target.value }))}
            />
          </label>
          <label>
            Password
            <input
              type="password"
              required
              minLength={8}
              value={form.password}
              onChange={(e) => setForm((s) => ({ ...s, password: e.target.value }))}
            />
          </label>
          <label>
            Role
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
          <button className="btn btn-primary" type="submit">
            Create
          </button>
        </form>
      </div>

      {flash && <p className="status-ok">{flash}</p>}
      {error && <p className="status-error">{error}</p>}

      <div className="panel">
        <h3>Users ({pageData.totalElements || 0})</h3>
        <div className="table-wrap">
          <table>
            <thead>
              <tr>
                <th>ID</th>
                <th>Username</th>
                <th>Email</th>
                <th>Role</th>
                <th>Active</th>
                <th>Actions</th>
              </tr>
            </thead>
            <tbody>
              {(pageData.items || []).map((user) => (
                <tr key={user.id}>
                  <td>{user.id}</td>
                  <td>{user.username}</td>
                  <td>{user.email || '-'}</td>
                  <td>
                    <div className="inline-form">
                      <select
                        value={rowRoleDraft[user.id] || user.role}
                        onChange={(e) =>
                          setRowRoleDraft((s) => ({ ...s, [user.id]: e.target.value }))
                        }
                      >
                        {ROLE_OPTIONS.map((role) => (
                          <option key={role} value={role}>
                            {role}
                          </option>
                        ))}
                      </select>
                      <button className="btn btn-outline" onClick={() => onUpdateRole(user.id)}>
                        Save
                      </button>
                    </div>
                  </td>
                  <td>{user.active ? 'Yes' : 'No'}</td>
                  <td>
                    <div className="inline-form">
                      <button className="btn btn-outline" onClick={() => onToggleStatus(user)}>
                        {user.active ? 'Deactivate' : 'Activate'}
                      </button>
                      <button
                        className="btn btn-outline"
                        onClick={() => onClearPermissions(user.id)}
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
      </div>
    </div>
  )
}
