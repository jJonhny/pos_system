import { useState } from 'react'
import { Navigate, useLocation, useNavigate } from 'react-router-dom'
import { loginPassword, verifyOtp } from '../../../shared/api/endpoints/authApi'
import { useAuth } from '../useAuth'

export function LoginPage() {
  const { isAuthenticated, completeLogin } = useAuth()
  const navigate = useNavigate()
  const location = useLocation()
  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')
  const [otpCode, setOtpCode] = useState('')
  const [challenge, setChallenge] = useState(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState('')
  const [statusText, setStatusText] = useState('')

  if (isAuthenticated) {
    return <Navigate to="/admin" replace />
  }

  const from = location.state?.from?.pathname || '/admin'

  const submitPassword = async (e) => {
    e.preventDefault()
    setError('')
    setStatusText('')
    setLoading(true)
    try {
      const result = await loginPassword({ email, password })
      if (result.challengeToken) {
        setChallenge(result)
        setStatusText(result.message || 'OTP verification required.')
      } else {
        setError(result.message || `Login blocked (${result.status || 'UNKNOWN'}).`)
      }
    } catch (err) {
      setError(err.message || 'Login request failed.')
    } finally {
      setLoading(false)
    }
  }

  const submitOtp = async (e) => {
    e.preventDefault()
    setError('')
    setLoading(true)
    try {
      const result = await verifyOtp({
        challengeToken: challenge.challengeToken,
        otpCode
      })
      if (!result.accessToken) {
        setError(result.message || `OTP rejected (${result.status || 'UNKNOWN'}).`)
        return
      }
      await completeLogin(result)
      navigate(from, { replace: true })
    } catch (err) {
      setError(err.message || 'OTP verification failed.')
    } finally {
      setLoading(false)
    }
  }

  return (
    <div className="login-shell">
      <div className="login-card">
        <h1>POS Admin</h1>
        <p>Sign in with your API account.</p>

        {!challenge && (
          <form onSubmit={submitPassword} className="stack-form">
            <label>
              Email
              <input
                value={email}
                onChange={(e) => setEmail(e.target.value)}
                type="email"
                required
                placeholder="admin@example.com"
              />
            </label>
            <label>
              Password
              <input
                value={password}
                onChange={(e) => setPassword(e.target.value)}
                type="password"
                required
                placeholder="********"
              />
            </label>
            <button disabled={loading} className="btn btn-primary" type="submit">
              {loading ? 'Signing in...' : 'Continue'}
            </button>
          </form>
        )}

        {challenge && (
          <form onSubmit={submitOtp} className="stack-form">
            <label>
              OTP Code
              <input
                value={otpCode}
                onChange={(e) => setOtpCode(e.target.value)}
                type="text"
                inputMode="numeric"
                minLength={6}
                maxLength={6}
                required
                placeholder="123456"
              />
            </label>
            {challenge.firstTimeSetup && challenge.otpauthUrl && (
              <p className="hint">
                First-time setup required. Configure your authenticator app using the provided QR/OTP data from backend.
              </p>
            )}
            <button disabled={loading} className="btn btn-primary" type="submit">
              {loading ? 'Verifying...' : 'Verify OTP'}
            </button>
            <button
              type="button"
              className="btn btn-outline"
              disabled={loading}
              onClick={() => {
                setChallenge(null)
                setOtpCode('')
              }}
            >
              Back
            </button>
          </form>
        )}

        {statusText && <p className="status-ok">{statusText}</p>}
        {error && <p className="status-error">{error}</p>}
      </div>
    </div>
  )
}
