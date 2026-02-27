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
        <h2>Inventory</h2>
        <p>Backed by `/api/v1/inventory/movements`, `/adjustments`, `/receive`</p>
      </div>

      {flash && <p className="status-ok">{flash}</p>}
      {error && <p className="status-error">{error}</p>}

      <div className="card-grid">
        <div className="panel">
          <h3>Movement Filters</h3>
          <form className="stack-form" onSubmit={(e) => e.preventDefault()}>
            <label>
              Product ID
              <input
                type="number"
                value={filters.productId}
                onChange={(e) => setFilters((s) => ({ ...s, productId: e.target.value }))}
              />
            </label>
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
              Type
              <select
                value={filters.type}
                onChange={(e) => setFilters((s) => ({ ...s, type: e.target.value }))}
              >
                <option value="">All</option>
                <option value="SALE">SALE</option>
                <option value="RETURN">RETURN</option>
                <option value="VOID">VOID</option>
                <option value="RECEIVE">RECEIVE</option>
                <option value="ADJUSTMENT">ADJUSTMENT</option>
                <option value="TRANSFER">TRANSFER</option>
                <option value="IMPORT">IMPORT</option>
              </select>
            </label>
            <div className="inline-form">
              <button className="btn btn-outline" onClick={loadMovements}>
                Load Movements
              </button>
              <button className="btn btn-outline" onClick={lookupAvailability}>
                Check Availability
              </button>
            </div>
          </form>
        </div>

        <div className="panel">
          <h3>Adjust Stock</h3>
          <form className="stack-form" onSubmit={submitAdjustment}>
            <label>
              Product ID
              <input
                type="number"
                required
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
                <option value="DELTA">DELTA</option>
                <option value="TARGET">TARGET</option>
              </select>
            </label>
            <label>
              Quantity
              <input
                type="number"
                required
                value={adjustForm.quantity}
                onChange={(e) => setAdjustForm((s) => ({ ...s, quantity: e.target.value }))}
              />
            </label>
            <label>
              Reason
              <input
                value={adjustForm.reason}
                onChange={(e) => setAdjustForm((s) => ({ ...s, reason: e.target.value }))}
              />
            </label>
            <button className="btn btn-primary" type="submit">
              Apply Adjustment
            </button>
          </form>
        </div>

        <div className="panel">
          <h3>Receive Stock</h3>
          <form className="stack-form" onSubmit={submitReceive}>
            <label>
              Product ID
              <input
                type="number"
                required
                value={receiveForm.productId}
                onChange={(e) => setReceiveForm((s) => ({ ...s, productId: e.target.value }))}
              />
            </label>
            <label>
              Quantity
              <input
                type="number"
                min={1}
                required
                value={receiveForm.quantity}
                onChange={(e) => setReceiveForm((s) => ({ ...s, quantity: e.target.value }))}
              />
            </label>
            <label>
              Notes
              <input
                value={receiveForm.notes}
                onChange={(e) => setReceiveForm((s) => ({ ...s, notes: e.target.value }))}
              />
            </label>
            <button className="btn btn-primary" type="submit">
              Receive
            </button>
          </form>
        </div>
      </div>

      {availability && (
        <div className="panel">
          <h3>Availability</h3>
          <p>
            Product <strong>{availability.productName}</strong> (ID {availability.productId}) has
            stock <strong>{availability.stockQty ?? 0}</strong>.
          </p>
        </div>
      )}

      <div className="panel">
        <h3>Movements</h3>
        <div className="table-wrap">
          <table>
            <thead>
              <tr>
                <th>When</th>
                <th>Product</th>
                <th>Type</th>
                <th>Delta</th>
                <th>Ref</th>
                <th>Notes</th>
              </tr>
            </thead>
            <tbody>
              {movements.map((m) => (
                <tr key={m.id}>
                  <td>{m.createdAt || '-'}</td>
                  <td>
                    {m.productName} (#{m.productId})
                  </td>
                  <td>{m.type}</td>
                  <td>{m.qtyDelta}</td>
                  <td>
                    {m.refType}/{m.refId}
                  </td>
                  <td>{m.notes || '-'}</td>
                </tr>
              ))}
              {movements.length === 0 && (
                <tr>
                  <td colSpan={6}>No movement rows loaded.</td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}
