import { Link } from 'react-router-dom'

export function NotFoundPage() {
  return (
    <div className="auth-page">
      <div className="panel narrow">
        <h2>Page Not Found</h2>
        <p>The route does not exist in the React Admin app.</p>
        <Link className="btn btn-primary" to="/admin">
          Go to Dashboard
        </Link>
      </div>
    </div>
  )
}
