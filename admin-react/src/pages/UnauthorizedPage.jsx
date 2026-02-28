import { Link } from 'react-router-dom'

export function UnauthorizedPage() {
  return (
    <div className="auth-page">
      <div className="login-card" style={{ textAlign: 'center' }}>
        <div style={{ fontSize: '4rem', marginBottom: '16px' }}>🔐</div>
        <h1 style={{ margin: '0 0 8px', fontSize: '2.5rem', color: 'var(--text)' }}>403</h1>
        <h2 style={{ margin: '0 0 16px', fontSize: '1.25rem', fontWeight: 600, color: 'var(--text-secondary)' }}>
          Access Denied
        </h2>
        <p style={{ margin: '0 0 24px', color: 'var(--muted)', fontSize: '0.95rem' }}>
          Your current role doesn't have permission to access this page. Contact your administrator if you believe this is an error.
        </p>
        <Link className="btn btn-primary" to="/admin" style={{ width: '100%', justifyContent: 'center' }}>
          ← Return to Dashboard
        </Link>
      </div>
    </div>
  )
}
