import { httpClient } from '../httpClient'
import { unwrapApiResponse } from '../apiResult'

export async function loginPassword(payload) {
  const res = await httpClient.post('/api/v1/auth/login', payload)
  return unwrapApiResponse(res.data)
}

export async function verifyOtp(payload) {
  const res = await httpClient.post('/api/v1/auth/verify-otp', payload)
  return unwrapApiResponse(res.data)
}

export async function me() {
  const res = await httpClient.get('/api/v1/auth/me')
  return unwrapApiResponse(res.data)
}
