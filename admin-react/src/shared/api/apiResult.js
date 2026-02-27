export function unwrapApiResponse(payload) {
  if (payload && typeof payload === 'object' && 'success' in payload) {
    if (payload.success === false) {
      const message = payload.message || 'API request failed.'
      const details = Array.isArray(payload.details) ? payload.details : []
      const err = new Error(message)
      err.details = details
      err.code = payload.code || 'API_ERROR'
      throw err
    }
    return payload.data
  }
  return payload
}
