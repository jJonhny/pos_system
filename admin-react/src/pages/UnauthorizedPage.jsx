import { Link } from 'react-router-dom'

export function UnauthorizedPage() {
  return (
    <div className="auth-page">
      <div className="panel narrow">
        <h2>Unauthorized</h2>
        <p>Your role cannot access this page.</p>
        <Link className="btn btn-primary" to="/admin">
          Back to Dashboard
        </Link>
      </div>
    </div>
  )
}
