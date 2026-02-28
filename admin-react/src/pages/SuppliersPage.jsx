import { useEffect, useState } from 'react'
import {
  createSupplier,
  deleteSupplier,
  listSuppliers,
  updateSupplier
} from '../shared/api/endpoints/suppliersApi'
import { Spinner } from '../shared/ui/Spinner'

const STATUS_OPTIONS = ['ACTIVE', 'INACTIVE']
const EMPTY_FORM = {
  name: '',
  phone: '',
  email: '',
  address: '',
  status: 'ACTIVE'
}

function normalizePayload(form) {
  const trim = (value) => {
    if (value === null || value === undefined) return null
    const normalized = String(value).trim()
    return normalized === '' ? null : normalized
  }
  return {
    name: trim(form.name) || '',
    phone: trim(form.phone),
    email: trim(form.email),
    address: trim(form.address),
    status: form.status || 'ACTIVE'
  }
}

export function SuppliersPage() {
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState('')
  const [flash, setFlash] = useState('')
  const [query, setQuery] = useState('')
  const [statusFilter, setStatusFilter] = useState('')
  const [items, setItems] = useState([])
  const [editingId, setEditingId] = useState(null)
  const [form, setForm] = useState(EMPTY_FORM)

  const load = async () => {
    setLoading(true)
    setError('')
    try {
      const data = await listSuppliers({
        q: query || undefined,
        status: statusFilter || undefined
      })
      setItems(Array.isArray(data) ? data : [])
    } catch (err) {
      setError(err.message || 'Failed to load suppliers.')
    } finally {
      setLoading(false)
    }
  }

  useEffect(() => {
    load()
  }, [])

  const resetForm = () => {
    setEditingId(null)
    setForm(EMPTY_FORM)
  }

  const onSubmit = async (e) => {
    e.preventDefault()
    setError('')
    setFlash('')
    const payload = normalizePayload(form)
    if (!payload.name) {
      setError('Supplier name is required.')
      return
    }

    try {
      if (editingId === null) {
        await createSupplier(payload)
        setFlash('Supplier created.')
      } else {
        await updateSupplier(editingId, payload)
        setFlash('Supplier updated.')
      }
      resetForm()
      await load()
    } catch (err) {
      setError(err.message || 'Failed to save supplier.')
    }
  }

  const onEdit = (row) => {
    setEditingId(row.id)
    setForm({
      name: row.name || '',
      phone: row.phone || '',
      email: row.email || '',
      address: row.address || '',
      status: row.status || 'ACTIVE'
    })
    setFlash('')
    setError('')
  }

  const onDelete = async (row) => {
    const ok = window.confirm(`Delete supplier "${row.name}"?`)
    if (!ok) return
    setError('')
    setFlash('')
    try {
      await deleteSupplier(row.id)
      if (editingId === row.id) {
        resetForm()
      }
      setFlash('Supplier deleted.')
      await load()
    } catch (err) {
      setError(err.message || 'Failed to delete supplier.')
    }
  }

  if (loading) return <Spinner label="Loading suppliers..." />

  return (
    <div className="page">
      <div className="page-head">
        <h2>Supplier Management</h2>
        <p>Manage supplier information, contact details, and business relationships</p>
      </div>

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>
          {editingId === null ? '➕ Add New Supplier' : `✏️ Edit Supplier #${editingId}`}
        </h3>
        <form className="stack-form" onSubmit={onSubmit}>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(2, 1fr)', gap: '20px' }}>
            <label style={{ display: 'grid', gap: '8px' }}>
              <span style={{ fontSize: '0.95rem', fontWeight: 600, color: 'var(--text)' }}>
                Company Name <span style={{ color: 'var(--bad)' }}>*</span>
              </span>
              <input
                required
                placeholder="Enter supplier name"
                value={form.name}
                onChange={(e) => setForm((s) => ({ ...s, name: e.target.value }))}
              />
            </label>
            <label style={{ display: 'grid', gap: '8px' }}>
              <span style={{ fontSize: '0.95rem', fontWeight: 600, color: 'var(--text)' }}>Phone Number</span>
              <input
                placeholder="Enter phone number"
                value={form.phone}
                onChange={(e) => setForm((s) => ({ ...s, phone: e.target.value }))}
              />
            </label>
            <label style={{ display: 'grid', gap: '8px' }}>
              <span style={{ fontSize: '0.95rem', fontWeight: 600, color: 'var(--text)' }}>Email Address</span>
              <input
                type="email"
                placeholder="Enter email address"
                value={form.email}
                onChange={(e) => setForm((s) => ({ ...s, email: e.target.value }))}
              />
            </label>
            <label style={{ display: 'grid', gap: '8px' }}>
              <span style={{ fontSize: '0.95rem', fontWeight: 600, color: 'var(--text)' }}>Status</span>
              <select
                value={form.status}
                onChange={(e) => setForm((s) => ({ ...s, status: e.target.value }))}
              >
                {STATUS_OPTIONS.map((status) => (
                  <option key={status} value={status}>
                    {status === 'ACTIVE' ? '✓ Active' : '✕ Inactive'}
                  </option>
                ))}
              </select>
            </label>
          </div>
          <label style={{ display: 'grid', gap: '8px', marginTop: '16px' }}>
            <span style={{ fontSize: '0.95rem', fontWeight: 600, color: 'var(--text)' }}>Address</span>
            <textarea
              rows={3}
              placeholder="Enter full address"
              value={form.address}
              onChange={(e) => setForm((s) => ({ ...s, address: e.target.value }))}
              style={{ resize: 'vertical' }}
            />
          </label>
          <div style={{ display: 'flex', gap: '10px', marginTop: '12px' }}>
            <button className="btn btn-primary" type="submit">
              {editingId === null ? '✓ Create Supplier' : '✓ Save Changes'}
            </button>
            {editingId !== null && (
              <button className="btn btn-secondary" type="button" onClick={resetForm}>
                Cancel Edit
              </button>
            )}
          </div>
        </form>
      </div>

      <div className="panel">
        <h3 style={{ margin: '0 0 16px', fontSize: '1.1rem', fontWeight: 700 }}>🔍 Search & Filter</h3>
        <form className="stack-form" onSubmit={(e) => e.preventDefault()}>
          <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))', gap: '12px' }}>
            <input
              placeholder="Search by supplier name..."
              value={query}
              onChange={(e) => setQuery(e.target.value)}
            />
            <select value={statusFilter} onChange={(e) => setStatusFilter(e.target.value)}>
              <option value="">All statuses</option>
              {STATUS_OPTIONS.map((status) => (
                <option key={status} value={status}>
                  {status === 'ACTIVE' ? '✓ Active' : '✕ Inactive'}
                </option>
              ))}
            </select>
            <button className="btn btn-primary" onClick={load}>
              Search
            </button>
          </div>
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
          🏢 Suppliers <span style={{ fontSize: '0.85rem', color: 'var(--muted)', fontWeight: 500 }}>({items.length})</span>
        </h3>
        {items.length === 0 ? (
          <div style={{ padding: '32px', textAlign: 'center', color: 'var(--muted)' }}>
            <p style={{ margin: 0, fontSize: '1rem', fontWeight: 500 }}>No suppliers found</p>
            <p style={{ margin: '8px 0 0', fontSize: '0.9rem' }}>Create your first supplier to get started</p>
          </div>
        ) : (
          <div className="table-wrap">
            <table>
              <thead>
                <tr>
                  <th>ID</th>
                  <th>Name</th>
                  <th>Phone</th>
                  <th>Email</th>
                  <th>Status</th>
                  <th>Address</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {items.map((supplier) => (
                  <tr key={supplier.id}>
                    <td style={{ fontWeight: 600, color: 'var(--brand)' }}>{supplier.id}</td>
                    <td style={{ fontWeight: 600 }}>{supplier.name}</td>
                    <td style={{ fontSize: '0.9rem', color: 'var(--muted)' }}>{supplier.phone || '—'}</td>
                    <td style={{ fontSize: '0.9rem', color: 'var(--muted)' }}>{supplier.email || '—'}</td>
                    <td>
                      <span
                        style={{
                          display: 'inline-block',
                          padding: '4px 10px',
                          borderRadius: '6px',
                          fontSize: '0.85rem',
                          fontWeight: 600,
                          backgroundColor: supplier.status === 'ACTIVE' ? 'var(--good-light)' : 'var(--bad-light)',
                          color: supplier.status === 'ACTIVE' ? 'var(--good)' : 'var(--bad)'
                        }}
                      >
                        {supplier.status === 'ACTIVE' ? '✓ Active' : '✕ Inactive'}
                      </span>
                    </td>
                    <td style={{ fontSize: '0.85rem', color: 'var(--muted)', maxWidth: '150px', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
                      {supplier.address || '—'}
                    </td>
                    <td>
                      <div style={{ display: 'flex', gap: '6px', flexWrap: 'wrap' }}>
                        <button
                          className="btn btn-secondary"
                          onClick={() => onEdit(supplier)}
                          style={{ fontSize: '0.85rem', padding: '8px 12px' }}
                        >
                          Edit
                        </button>
                        <button
                          className="btn btn-secondary"
                          onClick={() => onDelete(supplier)}
                          style={{ fontSize: '0.85rem', padding: '8px 12px', color: 'var(--bad)' }}
                        >
                          Delete
                        </button>
                      </div>
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
