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
        <p>Live data from `/api/v1/audit/events`</p>
      </div>

      {error && <p className="status-error">{error}</p>}

      <div className="panel">
        <form className="inline-form" onSubmit={(e) => e.preventDefault()}>
          <label>
            From
            <input
              type="date"
              value={filters.from}
              onChange={(e) => setFilters((s) => ({ ...s, from: e.target.value }))}
            />
          </label>
          <label>
            To
            <input
              type="date"
              value={filters.to}
              onChange={(e) => setFilters((s) => ({ ...s, to: e.target.value }))}
            />
          </label>
          <label>
            User
            <input
              value={filters.user}
              onChange={(e) => setFilters((s) => ({ ...s, user: e.target.value }))}
            />
          </label>
          <label>
            Action
            <select
              value={filters.actionType}
              onChange={(e) => setFilters((s) => ({ ...s, actionType: e.target.value }))}
            >
              <option value="">All</option>
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
              <option value="">All</option>
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
              value={filters.targetId}
              onChange={(e) => setFilters((s) => ({ ...s, targetId: e.target.value }))}
            />
          </label>
          <button className="btn btn-outline" onClick={load}>
            Search
          </button>
        </form>
      </div>

      <div className="panel">
        <div className="table-wrap">
          <table>
            <thead>
              <tr>
                <th>Time</th>
                <th>User</th>
                <th>Action</th>
                <th>Target</th>
                <th>Target ID</th>
                <th>IP</th>
                <th>Terminal</th>
              </tr>
            </thead>
            <tbody>
              {rows.map((row) => (
                <tr key={row.id}>
                  <td>{row.timestamp || '-'}</td>
                  <td>{row.actorUsername || '-'}</td>
                  <td>{row.actionType || '-'}</td>
                  <td>{row.targetType || '-'}</td>
                  <td>{row.targetId || '-'}</td>
                  <td>{row.ipAddress || '-'}</td>
                  <td>{row.terminalId || '-'}</td>
                </tr>
              ))}
              {rows.length === 0 && (
                <tr>
                  <td colSpan={7}>No audit rows found.</td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}
