import { useEffect, useState } from 'react'
import { listUsers } from '../shared/api/endpoints/usersApi'
import { listProducts } from '../shared/api/endpoints/productsApi'
import { listMovements } from '../shared/api/endpoints/inventoryApi'
import { Spinner } from '../shared/ui/Spinner'

export function DashboardPage() {
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState('')
  const [stats, setStats] = useState({
    users: 0,
    products: 0,
    lowStock: 0,
    recentMovements: 0
  })

  useEffect(() => {
    let active = true
    const load = async () => {
      setLoading(true)
      setError('')
      try {
        const [users, products, lowStock, movements] = await Promise.all([
          listUsers({ page: 0, size: 1 }),
          listProducts({ page: 0, size: 1 }),
          listProducts({ page: 0, size: 1, lowStock: true }),
          listMovements({ page: 0, size: 5 })
        ])
        if (!active) return
        setStats({
          users: users?.totalElements || 0,
          products: products?.totalElements || 0,
          lowStock: lowStock?.totalElements || 0,
          recentMovements: movements?.items?.length || 0
        })
      } catch (err) {
        if (!active) return
        setError(err.message || 'Failed to load dashboard metrics.')
      } finally {
        if (active) setLoading(false)
      }
    }
    load()
    return () => {
      active = false
    }
  }, [])

  if (loading) return <Spinner label="Loading dashboard..." />

  return (
    <div className="page">
      <div className="page-head">
        <h2>Dashboard</h2>
        <p>System overview and key metrics from your POS system</p>
      </div>

      {error && (
        <div className="status-error" style={{ margin: '16px 0' }}>
          <strong>⚠️ Error:</strong> {error}
        </div>
      )}

      <div className="card-grid">
        <article className="metric-card" style={{ borderLeft: '4px solid var(--brand)' }}>
          <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" style={{ color: 'var(--brand)', marginBottom: '8px' }}>
            <path d="M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2"></path>
            <circle cx="12" cy="7" r="4"></circle>
          </svg>
          <h3>Total Users</h3>
          <strong>{stats.users.toLocaleString()}</strong>
          <p style={{ fontSize: '0.85rem', color: 'var(--muted)', margin: '8px 0 0' }}>Active user accounts</p>
        </article>

        <article className="metric-card" style={{ borderLeft: '4px solid var(--info)' }}>
          <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" style={{ color: 'var(--info)', marginBottom: '8px' }}>
            <rect x="3" y="3" width="18" height="18" rx="2"></rect>
            <path d="M7 7h10"></path>
            <path d="M7 11h10"></path>
            <path d="M7 15h4"></path>
          </svg>
          <h3>Products</h3>
          <strong>{stats.products.toLocaleString()}</strong>
          <p style={{ fontSize: '0.85rem', color: 'var(--muted)', margin: '8px 0 0' }}>Items in catalog</p>
        </article>

        <article className="metric-card" style={{ borderLeft: '4px solid var(--warning)' }}>
          <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" style={{ color: 'var(--warning)', marginBottom: '8px' }}>
            <path d="M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3.05h16.94a2 2 0 0 0 1.71-3.05L13.71 3.86a2 2 0 0 0-3.42 0z"></path>
            <line x1="12" y1="9" x2="12" y2="13"></line>
            <line x1="12" y1="17" x2="12.01" y2="17"></line>
          </svg>
          <h3>Low Stock</h3>
          <strong>{stats.lowStock}</strong>
          <p style={{ fontSize: '0.85rem', color: 'var(--muted)', margin: '8px 0 0' }}>Items below threshold</p>
        </article>

        <article className="metric-card" style={{ borderLeft: '4px solid var(--good)' }}>
          <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" style={{ color: 'var(--good)', marginBottom: '8px' }}>
            <polyline points="12 3 20 7.5 20 16.5 12 21 4 16.5 4 7.5 12 3"></polyline>
            <line x1="12" y1="12" x2="20" y2="7.5"></line>
            <line x1="12" y1="12" x2="12" y2="21"></line>
            <line x1="12" y1="12" x2="4" y2="7.5"></line>
          </svg>
          <h3>Recent Movements</h3>
          <strong>{stats.recentMovements}</strong>
          <p style={{ fontSize: '0.85rem', color: 'var(--muted)', margin: '8px 0 0' }}>Latest inventory changes</p>
        </article>
      </div>
    </div>
  )
}
