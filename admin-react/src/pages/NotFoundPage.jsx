import { Link } from 'react-router-dom'

export function NotFoundPage() {
  return (
    <div className="auth-page">
      <div className="login-card" style={{ textAlign: 'center' }}>
        <div style={{ fontSize: '4rem', marginBottom: '16px' }}>🔍</div>
        <h1 style={{ margin: '0 0 8px', fontSize: '2.5rem', color: 'var(--text)' }}>404</h1>
        <h2 style={{ margin: '0 0 16px', fontSize: '1.25rem', fontWeight: 600, color: 'var(--text-secondary)' }}>
          Page Not Found
        </h2>
        <p style={{ margin: '0 0 24px', color: 'var(--muted)', fontSize: '0.95rem' }}>
          The route you're looking for doesn't exist in the React Admin app. Let's get you back on track.
        </p>
        <Link className="btn btn-primary" to="/admin" style={{ width: '100%', justifyContent: 'center' }}>
          ← Return to Dashboard
        </Link>
      </div>
    </div>
  )
}
