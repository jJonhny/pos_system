import { useState } from 'react'
import {
  adjustStock,
  getAvailability,
  listMovements,
  receiveStock
} from '../shared/api/endpoints/inventoryApi'

export function InventoryPage() {
  const [error, setError] = useState('')
  const [flash, setFlash] = useState('')
  const [movements, setMovements] = useState([])
  const [availability, setAvailability] = useState(null)
  const [filters, setFilters] = useState({ productId: '', from: '', to: '', type: '' })
  const [adjustForm, setAdjustForm] = useState({
    productId: '',
    mode: 'DELTA',
    quantity: '',
    reason: ''
  })
  const [receiveForm, setReceiveForm] = useState({
    productId: '',
    quantity: '',
    notes: ''
  })

  const loadMovements = async () => {
    setError('')
    setFlash('')
    try {
      const data = await listMovements({
        page: 0,
        size: 30,
        productId: filters.productId || undefined,
        from: filters.from || undefined,
        to: filters.to || undefined,
        type: filters.type || undefined
      })
      setMovements(data?.items || [])
    } catch (err) {
      setError(err.message || 'Failed to load movements.')
    }
  }

  const lookupAvailability = async () => {
    setError('')
    setFlash('')
    try {
      if (!filters.productId) {
        setError('Enter product ID first.')
        return
      }
      const data = await getAvailability(Number(filters.productId))
      setAvailability(data)
    } catch (err) {
      setError(err.message || 'Failed to load availability.')
    }
  }

  const submitAdjustment = async (e) => {
    e.preventDefault()
    setError('')
    setFlash('')
    try {
      const payload = {
        productId: Number(adjustForm.productId),
        mode: adjustForm.mode,
        quantity: Number(adjustForm.quantity),
        reason: adjustForm.reason || undefined
      }
      const data = await adjustStock(payload)
      setAvailability(data)
      setFlash('Stock adjustment applied.')
      await loadMovements()
    } catch (err) {
      setError(err.message || 'Failed to adjust stock.')
    }
  }

  const submitReceive = async (e) => {
    e.preventDefault()
    setError('')
    setFlash('')
    try {
      const payload = {
        productId: Number(receiveForm.productId),
        quantity: Number(receiveForm.quantity),
        notes: receiveForm.notes || undefined
      }
      const data = await receiveStock(payload)
      setAvailability(data)
      setFlash('Stock received successfully.')
      await loadMovements()
    } catch (err) {
      setError(err.message || 'Failed to receive stock.')
    }
  }

  return (
    <div className="page">
      <div className="page-head">
        <h2>Inventory Management</h2>
        <p>Track stock levels, adjust inventory, and manage warehouse operations</p>
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
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>📊 Movement Filters</h3>
        <form className="stack-form" onSubmit={(e) => e.preventDefault()}>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(180px, 1fr))', gap: '16px' }}>
            <label>
              Product ID
              <input
                type="number"
                placeholder="Enter product ID"
                value={filters.productId}
                onChange={(e) => setFilters((s) => ({ ...s, productId: e.target.value }))}
              />
            </label>
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
              Movement Type
              <select
                value={filters.type}
                onChange={(e) => setFilters((s) => ({ ...s, type: e.target.value }))}
              >
                <option value="">All Types</option>
                <option value="SALE">Sale</option>
                <option value="RETURN">Return</option>
                <option value="VOID">Void</option>
                <option value="RECEIVE">Receive</option>
                <option value="ADJUSTMENT">Adjustment</option>
                <option value="TRANSFER">Transfer</option>
                <option value="IMPORT">Import</option>
              </select>
            </label>
          </div>
          <div style={{ display: 'flex', gap: '10px', marginTop: '12px' }}>
            <button className="btn btn-primary" onClick={loadMovements}>
              📋 Load Movements
            </button>
            <button className="btn btn-secondary" onClick={lookupAvailability}>
              🔍 Check Availability
            </button>
          </div>
        </form>
      </div>

      <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))', gap: '20px', margin: '20px 0' }}>
        <div className="panel">
          <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>📦 Adjust Stock</h3>
          <form className="stack-form" onSubmit={submitAdjustment}>
            <label>
              Product ID <span style={{ color: 'var(--bad)' }}>*</span>
              <input
                type="number"
                required
                placeholder="Enter product ID"
                value={adjustForm.productId}
                onChange={(e) => setAdjustForm((s) => ({ ...s, productId: e.target.value }))}
              />
            </label>
            <label>
              Mode
              <select
                value={adjustForm.mode}
                onChange={(e) => setAdjustForm((s) => ({ ...s, mode: e.target.value }))}
              >
                <option value="DELTA">Delta (Add/Subtract)</option>
                <option value="TARGET">Target (Set to Exact)</option>
              </select>
            </label>
            <label>
              Quantity <span style={{ color: 'var(--bad)' }}>*</span>
              <input
                type="number"
                required
                placeholder="Enter quantity"
                value={adjustForm.quantity}
                onChange={(e) => setAdjustForm((s) => ({ ...s, quantity: e.target.value }))}
              />
            </label>
            <label>
              Reason
              <input
                placeholder="e.g., Damaged goods, Shrinkage"
                value={adjustForm.reason}
                onChange={(e) => setAdjustForm((s) => ({ ...s, reason: e.target.value }))}
              />
            </label>
            <button className="btn btn-primary" type="submit">
              ✓ Apply Adjustment
            </button>
          </form>
        </div>

        <div className="panel">
          <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>📥 Receive Stock</h3>
          <form className="stack-form" onSubmit={submitReceive}>
            <label>
              Product ID <span style={{ color: 'var(--bad)' }}>*</span>
              <input
                type="number"
                required
                placeholder="Enter product ID"
                value={receiveForm.productId}
                onChange={(e) => setReceiveForm((s) => ({ ...s, productId: e.target.value }))}
              />
            </label>
            <label>
              Quantity <span style={{ color: 'var(--bad)' }}>*</span>
              <input
                type="number"
                min={1}
                required
                placeholder="Enter quantity"
                value={receiveForm.quantity}
                onChange={(e) => setReceiveForm((s) => ({ ...s, quantity: e.target.value }))}
              />
            </label>
            <label>
              Notes
              <input
                placeholder="e.g., PO#12345, Supplier ABC"
                value={receiveForm.notes}
                onChange={(e) => setReceiveForm((s) => ({ ...s, notes: e.target.value }))}
              />
            </label>
            <button className="btn btn-primary" type="submit">
              ✓ Receive Stock
            </button>
          </form>
        </div>
      </div>

      {availability && (
        <div className="panel" style={{ backgroundColor: 'var(--good-light)', borderLeft: '4px solid var(--good)' }}>
          <h3 style={{ margin: '0 0 12px', fontSize: '1.1rem', fontWeight: 700, color: 'var(--good)' }}>
            ✓ Current Availability
          </h3>
          <p style={{ margin: '0', fontSize: '1rem', color: 'var(--text)' }}>
            Product <strong style={{ color: 'var(--good)' }}>{availability.productName}</strong> (ID <strong>{availability.productId}</strong>) has a current stock level of <strong style={{ fontSize: '1.25rem', color: 'var(--good)' }}>{availability.stockQty ?? 0}</strong> units.
          </p>
        </div>
      )}

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>
          🔄 Stock Movements <span style={{ fontSize: '0.85rem', color: 'var(--muted)', fontWeight: 500 }}>({movements.length})</span>
        </h3>
        {movements.length === 0 ? (
          <div style={{ padding: '32px', textAlign: 'center', color: 'var(--muted)' }}>
            <p style={{ margin: 0, fontSize: '1rem', fontWeight: 500 }}>No movements found</p>
            <p style={{ margin: '8px 0 0', fontSize: '0.9rem' }}>Adjust filters and try again</p>
          </div>
        ) : (
          <div className="table-wrap">
            <table>
              <thead>
                <tr>
                  <th>Date/Time</th>
                  <th>Product</th>
                  <th>Type</th>
                  <th>Quantity</th>
                  <th>Reference</th>
                  <th>Notes</th>
                </tr>
              </thead>
              <tbody>
                {movements.map((m) => (
                  <tr key={m.id}>
                    <td style={{ fontSize: '0.9rem', color: 'var(--muted)', fontWeight: 500 }}>
                      {m.createdAt ? new Date(m.createdAt).toLocaleString() : '—'}
                    </td>
                    <td style={{ fontWeight: 600 }}>
                      {m.productName} <span style={{ color: 'var(--brand)' }}>#{m.productId}</span>
                    </td>
                    <td>
                      <span
                        style={{
                          display: 'inline-block',
                          padding: '4px 10px',
                          borderRadius: '6px',
                          fontSize: '0.85rem',
                          fontWeight: 600,
                          backgroundColor:
                            m.type === 'RECEIVE'
                              ? 'var(--good-light)'
                              : m.type === 'SALE'
                                ? 'var(--brand-light)'
                                : m.type === 'ADJUSTMENT'
                                  ? 'var(--warning-light)'
                                  : 'var(--info-light)',
                          color:
                            m.type === 'RECEIVE'
                              ? 'var(--good)'
                              : m.type === 'SALE'
                                ? 'var(--brand)'
                                : m.type === 'ADJUSTMENT'
                                  ? 'var(--warning)'
                                  : 'var(--info)'
                        }}
                      >
                        {m.type}
                      </span>
                    </td>
                    <td style={{ fontWeight: 600, color: Math.sign(m.qtyDelta || 0) >= 0 ? 'var(--good)' : 'var(--bad)' }}>
                      {Math.sign(m.qtyDelta || 0) >= 0 ? '+' : ''}{m.qtyDelta}
                    </td>
                    <td style={{ fontSize: '0.9rem', color: 'var(--muted)' }}>
                      {m.refType}/{m.refId}
                    </td>
                    <td style={{ fontSize: '0.9rem', color: 'var(--muted)' }}>{m.notes || '—'}</td>
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
