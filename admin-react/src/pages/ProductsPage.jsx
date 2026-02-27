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
        <p>Backed by `GET/POST/PUT /api/v1/products/*`</p>
      </div>

      <div className="panel">
        <h3>Create Product</h3>
        <form className="stack-form" onSubmit={onCreate}>
          <label>
            Name
            <input
              required
              value={form.name}
              onChange={(e) => setForm((s) => ({ ...s, name: e.target.value }))}
            />
          </label>
          <label>
            SKU
            <input value={form.sku} onChange={(e) => setForm((s) => ({ ...s, sku: e.target.value }))} />
          </label>
          <label>
            Barcode
            <input
              value={form.barcode}
              onChange={(e) => setForm((s) => ({ ...s, barcode: e.target.value }))}
            />
          </label>
          <label>
            Price
            <input
              type="number"
              step="0.01"
              value={form.price}
              onChange={(e) => setForm((s) => ({ ...s, price: e.target.value }))}
            />
          </label>
          <label>
            Cost Price
            <input
              type="number"
              step="0.01"
              value={form.costPrice}
              onChange={(e) => setForm((s) => ({ ...s, costPrice: e.target.value }))}
            />
          </label>
          <label>
            Low Stock Threshold
            <input
              type="number"
              value={form.lowStockThreshold}
              onChange={(e) => setForm((s) => ({ ...s, lowStockThreshold: e.target.value }))}
            />
          </label>
          <label>
            Category ID (optional)
            <input
              type="number"
              value={form.categoryId}
              onChange={(e) => setForm((s) => ({ ...s, categoryId: e.target.value }))}
            />
          </label>
          <button className="btn btn-primary" type="submit">
            Create Product
          </button>
        </form>
      </div>

      <div className="panel">
        <form className="inline-form" onSubmit={(e) => e.preventDefault()}>
          <input
            placeholder="Search by name / SKU / barcode"
            value={query}
            onChange={(e) => setQuery(e.target.value)}
          />
          <button className="btn btn-outline" onClick={load}>
            Search
          </button>
        </form>
      </div>

      {flash && <p className="status-ok">{flash}</p>}
      {error && <p className="status-error">{error}</p>}

      <div className="panel">
        <h3>Products</h3>
        <div className="table-wrap">
          <table>
            <thead>
              <tr>
                <th>ID</th>
                <th>Name</th>
                <th>SKU</th>
                <th>Price</th>
                <th>Stock</th>
                <th>Low Stock</th>
                <th>Active</th>
                <th>Actions</th>
              </tr>
            </thead>
            <tbody>
              {items.map((p) => (
                <tr key={p.id}>
                  <td>{p.id}</td>
                  <td>{p.name}</td>
                  <td>{p.sku || '-'}</td>
                  <td>{p.price ?? '-'}</td>
                  <td>{p.stockQty ?? 0}</td>
                  <td>{p.lowStock ? 'Yes' : 'No'}</td>
                  <td>{p.active ? 'Yes' : 'No'}</td>
                  <td>
                    <button className="btn btn-outline" onClick={() => onToggleActive(p)}>
                      {p.active ? 'Disable' : 'Enable'}
                    </button>
                  </td>
                </tr>
              ))}
              {items.length === 0 && (
                <tr>
                  <td colSpan={8}>No products found.</td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}
