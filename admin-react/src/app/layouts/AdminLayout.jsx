import { NavLink, Outlet, useNavigate } from 'react-router-dom'
import { useAuth } from '../../features/auth/useAuth'
import { MANAGEMENT_ROLES, ROLES } from '../../shared/auth/roles'

function navClass({ isActive }) {
  return `side-link${isActive ? ' active' : ''}`
}

export function AdminLayout() {
  const { auth, logout } = useAuth()
  const navigate = useNavigate()

  const role = auth?.role
  const isAdmin = role === ROLES.ADMIN || role === ROLES.SUPER_ADMIN
  const isManager = MANAGEMENT_ROLES.includes(role)

  const onLogout = () => {
    logout()
    navigate('/login', { replace: true })
  }

  return (
    <div className="admin-shell">
      <aside className="sidebar">
        <div className="brand">
          <h1>POS Admin</h1>
          <p>React Console</p>
        </div>

        <nav className="side-nav">
          <NavLink to="/admin" end className={navClass}>
            Dashboard
          </NavLink>
          {isAdmin && (
            <NavLink to="/admin/users" className={navClass}>
              Users
            </NavLink>
          )}
          {isManager && (
            <>
              <NavLink to="/admin/products" className={navClass}>
                Products
              </NavLink>
              <NavLink to="/admin/inventory" className={navClass}>
                Inventory
              </NavLink>
              <NavLink to="/admin/suppliers" className={navClass}>
                Suppliers
              </NavLink>
              <NavLink to="/admin/reports" className={navClass}>
                Reports
              </NavLink>
            </>
          )}
          {isAdmin && (
            <NavLink to="/admin/audit" className={navClass}>
              Audit
            </NavLink>
          )}
        </nav>
      </aside>

      <main className="content-shell">
        <header className="topbar">
          <div>
            <strong>{auth?.username || 'Authenticated User'}</strong>
            <span className="topbar-role">{auth?.role || 'UNKNOWN'}</span>
          </div>
          <button className="btn btn-outline" onClick={onLogout}>
            Sign out
          </button>
        </header>
        <section className="page-content">
          <Outlet />
        </section>
      </main>
    </div>
  )
}
