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
        <h2>Reports</h2>
        <p>Live data from `/api/v1/reports/*`</p>
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
            Cashier
            <input
              value={filters.cashier}
              onChange={(e) => setFilters((s) => ({ ...s, cashier: e.target.value }))}
            />
          </label>
          <label>
            Terminal
            <input
              value={filters.terminal}
              onChange={(e) => setFilters((s) => ({ ...s, terminal: e.target.value }))}
            />
          </label>
          <button className="btn btn-outline" onClick={load}>
            Refresh
          </button>
        </form>
      </div>

      {summary && (
        <div className="card-grid">
          <article className="metric-card">
            <h3>Sales Count</h3>
            <strong>{summary.salesCount}</strong>
          </article>
          <article className="metric-card">
            <h3>Total Revenue</h3>
            <strong>{summary.totalRevenue}</strong>
          </article>
          <article className="metric-card">
            <h3>Average Ticket</h3>
            <strong>{summary.averageTicket}</strong>
          </article>
          <article className="metric-card">
            <h3>Shifts</h3>
            <strong>{summary.shiftCount}</strong>
          </article>
        </div>
      )}

      <div className="panel">
        <h3>Sales Rows</h3>
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
                  <td>{row.id}</td>
                  <td>{row.createdAt || '-'}</td>
                  <td>{row.cashierUsername || '-'}</td>
                  <td>{row.terminalId || '-'}</td>
                  <td>{row.status || '-'}</td>
                  <td>{row.total}</td>
                  <td>{row.refundedTotal}</td>
                </tr>
              ))}
              {sales.length === 0 && (
                <tr>
                  <td colSpan={7}>No sales rows.</td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      </div>

      <div className="panel">
        <h3>Shift Rows</h3>
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
                <th>Variance</th>
              </tr>
            </thead>
            <tbody>
              {shifts.map((row) => (
                <tr key={row.id}>
                  <td>{row.id}</td>
                  <td>{row.cashierUsername || '-'}</td>
                  <td>{row.terminalId || '-'}</td>
                  <td>{row.openedAt || '-'}</td>
                  <td>{row.closedAt || '-'}</td>
                  <td>{row.totalSales}</td>
                  <td>{row.varianceCash}</td>
                </tr>
              ))}
              {shifts.length === 0 && (
                <tr>
                  <td colSpan={7}>No shift rows.</td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}
