import { useEffect, useState } from 'react'
import { Navigate, useLocation, useNavigate } from 'react-router-dom'
import { loginPassword, verifyOtp } from '../../../shared/api/endpoints/authApi'
import { useAuth } from '../useAuth'

function decodeJwtExpiryMillis(token) {
  if (!token || typeof token !== 'string') {
    return null
  }
  const parts = token.split('.')
  if (parts.length !== 3) {
    return null
  }
  try {
    const base64 = parts[1].replace(/-/g, '+').replace(/_/g, '/')
    const paddedBase64 = base64.padEnd(base64.length + ((4 - (base64.length % 4)) % 4), '=')
    const payload = JSON.parse(atob(paddedBase64))
    if (!payload?.exp) {
      return null
    }
    return Number(payload.exp) * 1000
  } catch (_) {
    return null
  }
}

function formatCountdown(totalSeconds) {
  const safe = Number.isFinite(totalSeconds) ? Math.max(0, totalSeconds) : 0
  const minutes = Math.floor(safe / 60)
  const seconds = safe % 60
  return `${String(minutes).padStart(2, '0')}:${String(seconds).padStart(2, '0')}`
}

function extractSecretFromOtpAuthUrl(otpauthUrl) {
  if (!otpauthUrl || typeof otpauthUrl !== 'string') {
    return ''
  }
  try {
    const query = otpauthUrl.split('?')[1] || ''
    const params = new URLSearchParams(query)
    return params.get('secret') || ''
  } catch (_) {
    return ''
  }
}

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
  const [challengeExpiresAt, setChallengeExpiresAt] = useState(null)
  const [challengeNow, setChallengeNow] = useState(() => Date.now())

  if (isAuthenticated) {
    return <Navigate to="/admin" replace />
  }

  const from = location.state?.from?.pathname || '/admin'
  const challengeRemainingSeconds =
    challengeExpiresAt == null ? null : Math.max(0, Math.ceil((challengeExpiresAt - challengeNow) / 1000))
  const manualSecret = challenge?.otpauthUrl ? extractSecretFromOtpAuthUrl(challenge.otpauthUrl) : ''

  const resetOtpChallenge = () => {
    setChallenge(null)
    setOtpCode('')
    setChallengeExpiresAt(null)
  }

  useEffect(() => {
    if (challengeExpiresAt == null) {
      return undefined
    }
    const timerId = window.setInterval(() => {
      const now = Date.now()
      setChallengeNow(now)
      if (now >= challengeExpiresAt) {
        setChallenge(null)
        setOtpCode('')
        setChallengeExpiresAt(null)
        setStatusText('')
        setError('OTP session expired. Sign in with password again.')
      }
    }, 1000)
    return () => window.clearInterval(timerId)
  }, [challengeExpiresAt])

  const submitPassword = async (e) => {
    e.preventDefault()
    setError('')
    setStatusText('')
    setLoading(true)
    try {
      const result = await loginPassword({ email, password })
      if (result.challengeToken) {
        setChallenge(result)
        setOtpCode('')
        const expiresAt = decodeJwtExpiryMillis(result.challengeToken)
        setChallengeExpiresAt(expiresAt)
        setChallengeNow(Date.now())
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
    const normalizedOtp = otpCode.replace(/\D/g, '').slice(0, 6)
    if (!/^\d{6}$/.test(normalizedOtp)) {
      setLoading(false)
      setError('Enter a valid 6-digit OTP code.')
      return
    }
    try {
      const result = await verifyOtp({
        challengeToken: challenge.challengeToken,
        otpCode: normalizedOtp
      })
      if (!result.accessToken) {
        setError(result.message || `OTP rejected (${result.status || 'UNKNOWN'}).`)
        return
      }
      await completeLogin(result)
      navigate(from, { replace: true })
    } catch (err) {
      const message = err.message || 'OTP verification failed.'
      const normalizedMessage = message.toLowerCase()
      if (normalizedMessage.includes('otp session expired') || normalizedMessage.includes('invalid challenge token')) {
        resetOtpChallenge()
      }
      setError(message)
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
            {challengeRemainingSeconds != null && (
              <p className="hint otp-expiry-note">
                OTP session expires in <strong>{formatCountdown(challengeRemainingSeconds)}</strong>.
              </p>
            )}
            {challenge.firstTimeSetup && (
              <div className="otp-setup-card">
                <p className="hint">
                  First-time setup required. Scan the QR code with Google Authenticator and enter the current 6-digit code.
                </p>
                {challenge.qrDataUrl && (
                  <div className="otp-qr-wrap">
                    <img src={challenge.qrDataUrl} alt="OTP setup QR code" className="otp-qr" />
                  </div>
                )}
                {challenge.otpauthUrl && (
                  <label>
                    Setup URL
                    <input value={challenge.otpauthUrl} type="text" readOnly />
                  </label>
                )}
                {manualSecret && (
                  <label>
                    Manual Secret Key
                    <input value={manualSecret} type="text" readOnly />
                  </label>
                )}
              </div>
            )}
            <label>
              OTP Code
              <input
                value={otpCode}
                onChange={(e) => setOtpCode(e.target.value.replace(/\D/g, '').slice(0, 6))}
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
                You can use either the QR code or manual secret key above.
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
                resetOtpChallenge()
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
