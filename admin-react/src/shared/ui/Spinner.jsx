export function Spinner({ label = 'Loading...' }) {
  return (
    <div className="state-box">
      <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', gap: '16px' }}>
        <div className="spinner" />
        <p style={{ margin: 0, fontSize: '1rem', fontWeight: 500, color: 'var(--muted)' }}>{label}</p>
      </div>
    </div>
  )
}
