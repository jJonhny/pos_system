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
        <p>React admin console connected to `/api/v1/*`.</p>
      </div>

      {error && <p className="status-error">{error}</p>}

      <div className="card-grid">
        <article className="metric-card">
          <h3>Users</h3>
          <strong>{stats.users}</strong>
        </article>
        <article className="metric-card">
          <h3>Products</h3>
          <strong>{stats.products}</strong>
        </article>
        <article className="metric-card">
          <h3>Low Stock</h3>
          <strong>{stats.lowStock}</strong>
        </article>
        <article className="metric-card">
          <h3>Recent Movements</h3>
          <strong>{stats.recentMovements}</strong>
        </article>
      </div>
    </div>
  )
}
