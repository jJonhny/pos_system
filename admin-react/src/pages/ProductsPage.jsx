import { useEffect, useState } from 'react'
import { createProduct, listProducts, updateProduct } from '../shared/api/endpoints/productsApi'
import { Spinner } from '../shared/ui/Spinner'

function numberOrNull(value) {
  if (value === '' || value === null || value === undefined) return null
  const n = Number(value)
  return Number.isFinite(n) ? n : null
}

export function ProductsPage() {
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState('')
  const [flash, setFlash] = useState('')
  const [query, setQuery] = useState('')
  const [items, setItems] = useState([])
  const [form, setForm] = useState({
    name: '',
    sku: '',
    barcode: '',
    price: '',
    costPrice: '',
    lowStockThreshold: '5',
    categoryId: ''
  })

  const load = async () => {
    setLoading(true)
    setError('')
    try {
      const page = await listProducts({
        q: query || undefined,
        page: 0,
        size: 30,
        sort: 'id',
        dir: 'desc'
      })
      setItems(page?.items || [])
    } catch (err) {
      setError(err.message || 'Failed to load products.')
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
      await createProduct({
        name: form.name.trim(),
        sku: form.sku.trim() || null,
        barcode: form.barcode.trim() || null,
        price: numberOrNull(form.price),
        costPrice: numberOrNull(form.costPrice),
        lowStockThreshold: numberOrNull(form.lowStockThreshold),
        categoryId: numberOrNull(form.categoryId)
      })
      setFlash('Product created. Initial stock is 0 by design.')
      setForm({
        name: '',
        sku: '',
        barcode: '',
        price: '',
        costPrice: '',
        lowStockThreshold: '5',
        categoryId: ''
      })
      await load()
    } catch (err) {
      setError(err.message || 'Failed to create product.')
    }
  }

  const onToggleActive = async (row) => {
    setError('')
    setFlash('')
    try {
      await updateProduct(row.id, { active: !row.active })
      setFlash('Product status updated.')
      await load()
    } catch (err) {
      setError(err.message || 'Failed to update product.')
    }
  }

  if (loading) return <Spinner label="Loading products..." />

  return (
    <div className="page">
      <div className="page-head">
        <h2>Product Management</h2>
        <p>Create, manage, and track all products in your catalog</p>
      </div>

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>📝 Add New Product</h3>
        <form className="stack-form" onSubmit={onCreate}>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(4, 1fr)', gap: '20px' }}>
            <label style={{ display: 'grid', gap: '8px' }}>
              <span style={{ fontSize: '0.95rem', fontWeight: 600, color: 'var(--text)' }}>
                Product Name <span style={{ color: 'var(--bad)' }}>*</span>
              </span>
              <input
                required
                placeholder="Enter product name"
                value={form.name}
                onChange={(e) => setForm((s) => ({ ...s, name: e.target.value }))}
              />
            </label>
            <label style={{ display: 'grid', gap: '8px' }}>
              <span style={{ fontSize: '0.95rem', fontWeight: 600, color: 'var(--text)' }}>SKU</span>
              <input
                placeholder="Enter SKU"
                value={form.sku}
                onChange={(e) => setForm((s) => ({ ...s, sku: e.target.value }))}
              />
            </label>
            <label style={{ display: 'grid', gap: '8px' }}>
              <span style={{ fontSize: '0.95rem', fontWeight: 600, color: 'var(--text)' }}>Barcode</span>
              <input
                placeholder="Enter barcode"
                value={form.barcode}
                onChange={(e) => setForm((s) => ({ ...s, barcode: e.target.value }))}
              />
            </label>
            <label style={{ display: 'grid', gap: '8px' }}>
              <span style={{ fontSize: '0.95rem', fontWeight: 600, color: 'var(--text)' }}>Price</span>
              <input
                type="number"
                step="0.01"
                placeholder="0.00"
                value={form.price}
                onChange={(e) => setForm((s) => ({ ...s, price: e.target.value }))}
              />
            </label>
            <label style={{ display: 'grid', gap: '8px' }}>
              <span style={{ fontSize: '0.95rem', fontWeight: 600, color: 'var(--text)' }}>Cost Price</span>
              <input
                type="number"
                step="0.01"
                placeholder="0.00"
                value={form.costPrice}
                onChange={(e) => setForm((s) => ({ ...s, costPrice: e.target.value }))}
              />
            </label>
            <label style={{ display: 'grid', gap: '8px' }}>
              <span style={{ fontSize: '0.95rem', fontWeight: 600, color: 'var(--text)' }}>Low Stock Threshold</span>
              <input
                type="number"
                placeholder="5"
                value={form.lowStockThreshold}
                onChange={(e) => setForm((s) => ({ ...s, lowStockThreshold: e.target.value }))}
              />
            </label>
            <label style={{ display: 'grid', gap: '8px' }}>
              <span style={{ fontSize: '0.95rem', fontWeight: 600, color: 'var(--text)' }}>Category ID (optional)</span>
              <input
                type="number"
                placeholder="Enter category ID"
                value={form.categoryId}
                onChange={(e) => setForm((s) => ({ ...s, categoryId: e.target.value }))}
              />
            </label>
          </div>
          <div style={{ display: 'flex', gap: '8px', marginTop: '16px' }}>
            <button className="btn btn-primary" type="submit">
              ➕ Create Product
            </button>
            <button
              className="btn btn-secondary"
              type="button"
              onClick={() => setForm({ name: '', sku: '', barcode: '', price: '', costPrice: '', lowStockThreshold: '', categoryId: '' })}
            >
              Clear
            </button>
          </div>
        </form>
      </div>

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>🔍 Search & Filter</h3>
        <form className="inline-form" onSubmit={(e) => e.preventDefault()}>
          <input
            placeholder="Search by name, SKU, or barcode..."
            value={query}
            onFocus={(e) => { if (e.target.value === '') e.target.placeholder = ''; }}
            onBlur={(e) => { if (e.target.value === '') e.target.placeholder = 'Search by name, SKU, or barcode...'; }}
            onChange={(e) => setQuery(e.target.value)}
            style={{ flex: 1, minWidth: '200px' }}
          />
          <button className="btn btn-primary" onClick={load}>
            Search
          </button>
        </form>
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
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>
          📦 Products <span style={{ fontSize: '0.85rem', color: 'var(--muted)', fontWeight: 500 }}>({items.length})</span>
        </h3>
        {items.length === 0 ? (
          <div style={{ padding: '32px', textAlign: 'center', color: 'var(--muted)' }}>
            <p style={{ margin: 0, fontSize: '1rem', fontWeight: 500 }}>No products found</p>
            <p style={{ margin: '8px 0 0', fontSize: '0.9rem' }}>Create your first product to get started</p>
          </div>
        ) : (
          <div className="table-wrap">
            <table>
              <thead>
                <tr>
                  <th>ID</th>
                  <th>Name</th>
                  <th>SKU</th>
                  <th>Price</th>
                  <th>Stock</th>
                  <th>Status</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {items.map((p) => (
                  <tr key={p.id}>
                    <td style={{ fontWeight: 600, color: 'var(--brand)' }}>{p.id}</td>
                    <td>
                      <strong>{p.name}</strong>
                      {p.sku && <p style={{ fontSize: '0.85rem', color: 'var(--muted)', margin: '4px 0 0' }}>SKU: {p.sku}</p>}
                    </td>
                    <td>{p.sku || '—'}</td>
                    <td style={{ fontWeight: 600 }}>
                      {p.price ? `$${String(p.price).includes('.') ? p.price : p.price + '.00'}` : '—'}
                    </td>
                    <td>
                      <span
                        style={{
                          display: 'inline-block',
                          padding: '4px 10px',
                          borderRadius: '6px',
                          fontSize: '0.85rem',
                          fontWeight: 600,
                          backgroundColor: (p.stockQty ?? 0) <= (p.lowStockThreshold || 5) ? 'var(--warning-light)' : 'var(--good-light)',
                          color: (p.stockQty ?? 0) <= (p.lowStockThreshold || 5) ? 'var(--warning)' : 'var(--good)'
                        }}
                      >
                        {p.stockQty ?? 0}
                      </span>
                    </td>
                    <td>
                      <span
                        style={{
                          display: 'inline-block',
                          padding: '4px 10px',
                          borderRadius: '6px',
                          fontSize: '0.85rem',
                          fontWeight: 600,
                          backgroundColor: p.active ? 'var(--good-light)' : 'var(--bad-light)',
                          color: p.active ? 'var(--good)' : 'var(--bad)'
                        }}
                      >
                        {p.active ? '✓ Active' : '✕ Inactive'}
                      </span>
                    </td>
                    <td>
                      <button className="btn btn-secondary" onClick={() => onToggleActive(p)} style={{ fontSize: '0.9rem' }}>
                        {p.active ? 'Disable' : 'Enable'}
                      </button>
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
