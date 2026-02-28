import { useEffect, useState } from 'react'
import { auditEvents, auditFilterMeta } from '../shared/api/endpoints/auditApi'

export function AuditLogsPage() {
  const [filters, setFilters] = useState({
    from: '',
    to: '',
    user: '',
    actionType: '',
    targetType: '',
    targetId: ''
  })
  const [meta, setMeta] = useState({ actionTypes: [], targetTypes: [] })
  const [rows, setRows] = useState([])
  const [error, setError] = useState('')

  const load = async () => {
    setError('')
    try {
      const params = {
        from: filters.from || undefined,
        to: filters.to || undefined,
        user: filters.user || undefined,
        actionType: filters.actionType || undefined,
        targetType: filters.targetType || undefined,
        targetId: filters.targetId || undefined,
        page: 0,
        size: 50
      }
      const data = await auditEvents(params)
      setRows(data?.items || [])
    } catch (err) {
      setError(err.message || 'Failed to load audit events.')
    }
  }

  useEffect(() => {
    let active = true
    const init = async () => {
      try {
        const m = await auditFilterMeta()
        if (!active) return
        setMeta({
          actionTypes: m?.actionTypes || [],
          targetTypes: m?.targetTypes || []
        })
      } catch (_) {
        // Keep page usable even if meta endpoint fails.
      }
      if (active) {
        await load()
      }
    }
    init()
    return () => {
      active = false
    }
  }, [])

  return (
    <div className="page">
      <div className="page-head">
        <h2>Audit Logs</h2>
        <p>Track all system activities, user actions, and data changes</p>
      </div>

      {error && (
        <div className="status-error" style={{ margin: '16px 0' }}>
          <strong>⚠️</strong> {error}
        </div>
      )}

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>🔎 Filter Events</h3>
        <form className="stack-form" onSubmit={(e) => e.preventDefault()}>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(3, 1fr)', gap: '16px' }}>
            <label>
              From Date
              <input
                type="date"
                value={filters.from}
                onChange={(e) => setFilters((s) => ({ ...s, from: e.target.value }))}
              />
            </label>
            <label>
              To Date
              <input
                type="date"
                value={filters.to}
                onChange={(e) => setFilters((s) => ({ ...s, to: e.target.value }))}
              />
            </label>
            <label>
              Username
              <input
                placeholder="Filter by user"
                value={filters.user}
                onChange={(e) => setFilters((s) => ({ ...s, user: e.target.value }))}
              />
            </label>
            <label>
              Action Type
              <select
                value={filters.actionType}
                onChange={(e) => setFilters((s) => ({ ...s, actionType: e.target.value }))}
              >
                <option value="">All Actions</option>
                {meta.actionTypes.map((v) => (
                  <option key={v} value={v}>
                    {v}
                  </option>
                ))}
              </select>
            </label>
            <label>
              Target Type
              <select
                value={filters.targetType}
                onChange={(e) => setFilters((s) => ({ ...s, targetType: e.target.value }))}
              >
                <option value="">All Types</option>
                {meta.targetTypes.map((v) => (
                  <option key={v} value={v}>
                    {v}
                  </option>
                ))}
              </select>
            </label>
            <label>
              Target ID
              <input
                placeholder="Filter by ID"
                value={filters.targetId}
                onChange={(e) => setFilters((s) => ({ ...s, targetId: e.target.value }))}
              />
            </label>
          </div>
          <div style={{ display: 'flex', gap: '8px', marginTop: '8px' }}>
            <button className="btn btn-primary" onClick={load}>
              🔍 Search
            </button>
            <button
              className="btn btn-secondary"
              type="button"
              onClick={() => setFilters({ from: '', to: '', user: '', actionType: '', targetType: '', targetId: '' })}
            >
              Clear
            </button>
          </div>
        </form>
      </div>

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>
          📋 Events <span style={{ fontSize: '0.85rem', color: 'var(--muted)', fontWeight: 500 }}>({rows.length})</span>
        </h3>
        {rows.length === 0 ? (
          <div style={{ padding: '32px', textAlign: 'center', color: 'var(--muted)' }}>
            <p style={{ margin: 0, fontSize: '1rem', fontWeight: 500 }}>No audit events found</p>
            <p style={{ margin: '8px 0 0', fontSize: '0.9rem' }}>Try adjusting your filter criteria</p>
          </div>
        ) : (
          <div className="table-wrap">
            <table>
              <thead>
                <tr>
                  <th>Timestamp</th>
                  <th>User</th>
                  <th>Action</th>
                  <th>Target</th>
                  <th>Target ID</th>
                  <th>IP Address</th>
                  <th>Terminal</th>
                </tr>
              </thead>
              <tbody>
                {rows.map((row) => (
                  <tr key={row.id}>
                    <td style={{ fontSize: '0.9rem', color: 'var(--muted)', fontWeight: 500 }}>
                      {row.timestamp ? new Date(row.timestamp).toLocaleString() : '—'}
                    </td>
                    <td style={{ fontWeight: 600 }}>{row.actorUsername || '—'}</td>
                    <td>
                      <span
                        style={{
                          display: 'inline-block',
                          padding: '4px 10px',
                          borderRadius: '6px',
                          fontSize: '0.85rem',
                          fontWeight: 600,
                          backgroundColor: 'var(--brand-light)',
                          color: 'var(--brand)'
                        }}
                      >
                        {row.actionType || '—'}
                      </span>
                    </td>
                    <td>{row.targetType || '—'}</td>
                    <td style={{ color: 'var(--brand)', fontWeight: 600 }}>{row.targetId || '—'}</td>
                    <td style={{ fontSize: '0.85rem', color: 'var(--muted)' }}>{row.ipAddress || '—'}</td>
                    <td style={{ fontSize: '0.85rem', color: 'var(--muted)' }}>{row.terminalId || '—'}</td>
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
