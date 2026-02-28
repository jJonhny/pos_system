import { useEffect, useState } from 'react'
import { reportSales, reportShifts, reportSummary } from '../shared/api/endpoints/reportsApi'

export function ReportsPage() {
  const [filters, setFilters] = useState({
    from: '',
    to: '',
    cashier: '',
    terminal: ''
  })
  const [summary, setSummary] = useState(null)
  const [sales, setSales] = useState([])
  const [shifts, setShifts] = useState([])
  const [error, setError] = useState('')

  const load = async () => {
    setError('')
    try {
      const params = {
        from: filters.from || undefined,
        to: filters.to || undefined,
        cashier: filters.cashier || undefined,
        terminal: filters.terminal || undefined
      }
      const [s, salesPage, shiftsPage] = await Promise.all([
        reportSummary(params),
        reportSales({ from: params.from, to: params.to, page: 0, size: 20 }),
        reportShifts({ ...params, page: 0, size: 20 })
      ])
      setSummary(s)
      setSales(salesPage?.items || [])
      setShifts(shiftsPage?.items || [])
    } catch (err) {
      setError(err.message || 'Failed to load reports.')
    }
  }

  useEffect(() => {
    load()
  }, [])

  return (
    <div className="page">
      <div className="page-head">
        <h2>Reports & Analytics</h2>
        <p>Sales performance, revenue analysis, and shift reconciliation data</p>
      </div>

      {error && (
        <div className="status-error" style={{ margin: '16px 0' }}>
          <strong>⚠️</strong> {error}
        </div>
      )}

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>📊 Filter Reports</h3>
        <form className="stack-form" onSubmit={(e) => e.preventDefault()}>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(180px, 1fr))', gap: '16px' }}>
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
              Cashier
              <input
                placeholder="Filter by cashier"
                value={filters.cashier}
                onChange={(e) => setFilters((s) => ({ ...s, cashier: e.target.value }))}
              />
            </label>
            <label>
              Terminal
              <input
                placeholder="Filter by terminal"
                value={filters.terminal}
                onChange={(e) => setFilters((s) => ({ ...s, terminal: e.target.value }))}
              />
            </label>
          </div>
          <button className="btn btn-primary" onClick={load} style={{ marginTop: '8px' }}>
            📈 Generate Report
          </button>
        </form>
      </div>

      {summary && (
        <div className="card-grid">
          <article className="metric-card" style={{ borderLeft: '4px solid var(--good)' }}>
            <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" style={{ color: 'var(--good)', marginBottom: '8px' }}>
              <rect x="3" y="3" width="7" height="7"></rect>
              <rect x="14" y="3" width="7" height="7"></rect>
              <rect x="14" y="14" width="7" height="7"></rect>
              <rect x="3" y="14" width="7" height="7"></rect>
            </svg>
            <h3>Total Sales</h3>
            <strong>{summary.salesCount || 0}</strong>
            <p style={{ fontSize: '0.85rem', color: 'var(--muted)', margin: '8px 0 0' }}>Transaction count</p>
          </article>

          <article className="metric-card" style={{ borderLeft: '4px solid var(--brand)' }}>
            <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" style={{ color: 'var(--brand)', marginBottom: '8px' }}>
              <line x1="12" y1="1" x2="12" y2="23"></line>
              <path d="M17 5H9.5a3.5 3.5 0 0 0 0 7h5a3.5 3.5 0 0 1 0 7H6"></path>
            </svg>
            <h3>Total Revenue</h3>
            <strong>${(summary.totalRevenue || 0).toFixed(2)}</strong>
            <p style={{ fontSize: '0.85rem', color: 'var(--muted)', margin: '8px 0 0' }}>Gross sales</p>
          </article>

          <article className="metric-card" style={{ borderLeft: '4px solid var(--info)' }}>
            <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" style={{ color: 'var(--info)', marginBottom: '8px' }}>
              <path d="M21 15a2 2 0 0 1-2 2H7l-4 4V5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2z"></path>
            </svg>
            <h3>Avg. Ticket</h3>
            <strong>${(summary.averageTicket || 0).toFixed(2)}</strong>
            <p style={{ fontSize: '0.85rem', color: 'var(--muted)', margin: '8px 0 0' }}>Average per transaction</p>
          </article>

          <article className="metric-card" style={{ borderLeft: '4px solid var(--warning)' }}>
            <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" style={{ color: 'var(--warning)', marginBottom: '8px' }}>
              <circle cx="12" cy="12" r="1"></circle>
              <path d="M12 1v6m0 6v6"></path>
              <path d="M4.22 4.22l4.24 4.24m2.12 2.12l4.24 4.24"></path>
              <path d="M1 12h6m6 0h6"></path>
              <path d="M4.22 19.78l4.24-4.24m2.12-2.12l4.24-4.24"></path>
              <path d="M12 19v6"></path>
              <path d="M19.78 19.78l-4.24-4.24m-2.12-2.12l-4.24-4.24"></path>
              <path d="M23 12h-6"></path>
              <path d="M19.78 4.22l-4.24 4.24m-2.12 2.12l-4.24 4.24"></path>
            </svg>
            <h3>Shifts</h3>
            <strong>{summary.shiftCount || 0}</strong>
            <p style={{ fontSize: '0.85rem', color: 'var(--muted)', margin: '8px 0 0' }}>Completed shifts</p>
          </article>
        </div>
      )}

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>💳 Sales Details</h3>
        {sales.length === 0 ? (
          <div style={{ padding: '32px', textAlign: 'center', color: 'var(--muted)' }}>
            <p style={{ margin: 0, fontSize: '1rem', fontWeight: 500 }}>No sales data available</p>
            <p style={{ margin: '8px 0 0', fontSize: '0.9rem' }}>Adjust your filter criteria and try again</p>
          </div>
        ) : (
          <div className="table-wrap">
            <table>
              <thead>
                <tr>
                  <th>ID</th>
                  <th>Date</th>
                  <th>Cashier</th>
                  <th>Terminal</th>
                  <th>Status</th>
                  <th>Total</th>
                  <th>Refund</th>
                </tr>
              </thead>
              <tbody>
                {sales.map((row) => (
                  <tr key={row.id}>
                    <td style={{ fontWeight: 600, color: 'var(--brand)' }}>{row.id}</td>
                    <td style={{ fontSize: '0.9rem', color: 'var(--muted)', fontWeight: 500 }}>
                      {row.createdAt ? new Date(row.createdAt).toLocaleString() : '—'}
                    </td>
                    <td style={{ fontWeight: 600 }}>{row.cashierUsername || '—'}</td>
                    <td>{row.terminalId || '—'}</td>
                    <td>
                      <span
                        style={{
                          display: 'inline-block',
                          padding: '4px 10px',
                          borderRadius: '6px',
                          fontSize: '0.85rem',
                          fontWeight: 600,
                          backgroundColor: 'var(--good-light)',
                          color: 'var(--good)'
                        }}
                      >
                        {row.status || '—'}
                      </span>
                    </td>
                    <td style={{ fontWeight: 600, color: 'var(--good)' }}>${(row.total || 0).toFixed(2)}</td>
                    <td style={{ color: 'var(--bad)', fontWeight: 600 }}>
                      {row.refundedTotal ? `$${(row.refundedTotal).toFixed(2)}` : '—'}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>⏱️ Shift Reconciliation</h3>
        {shifts.length === 0 ? (
          <div style={{ padding: '32px', textAlign: 'center', color: 'var(--muted)' }}>
            <p style={{ margin: 0, fontSize: '1rem', fontWeight: 500 }}>No shift data available</p>
            <p style={{ margin: '8px 0 0', fontSize: '0.9rem' }}>Adjust your filter criteria and try again</p>
          </div>
        ) : (
          <div className="table-wrap">
            <table>
              <thead>
                <tr>
                  <th>ID</th>
                  <th>Cashier</th>
                  <th>Terminal</th>
                  <th>Opened</th>
                  <th>Closed</th>
                  <th>Total Sales</th>
                  <th>Cash Variance</th>
                </tr>
              </thead>
              <tbody>
                {shifts.map((row) => (
                  <tr key={row.id}>
                    <td style={{ fontWeight: 600, color: 'var(--brand)' }}>{row.id}</td>
                    <td style={{ fontWeight: 600 }}>{row.cashierUsername || '—'}</td>
                    <td>{row.terminalId || '—'}</td>
                    <td style={{ fontSize: '0.9rem', color: 'var(--muted)', fontWeight: 500 }}>
                      {row.openedAt ? new Date(row.openedAt).toLocaleString() : '—'}
                    </td>
                    <td style={{ fontSize: '0.9rem', color: 'var(--muted)', fontWeight: 500 }}>
                      {row.closedAt ? new Date(row.closedAt).toLocaleString() : '—'}
                    </td>
                    <td style={{ fontWeight: 600, color: 'var(--good)' }}>${(row.totalSales || 0).toFixed(2)}</td>
                    <td
                      style={{
                        fontWeight: 600,
                        color: Math.abs(row.varianceCash || 0) < 0.01 ? 'var(--good)' : 'var(--warning)'
                      }}
                    >
                      ${(row.varianceCash || 0).toFixed(2)}
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
