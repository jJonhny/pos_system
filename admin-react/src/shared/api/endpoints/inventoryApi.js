import { httpClient } from '../httpClient'
import { unwrapApiResponse } from '../apiResult'

export async function listMovements(params = {}) {
  const res = await httpClient.get('/api/v1/inventory/movements', { params })
  return unwrapApiResponse(res.data)
}

export async function getAvailability(productId) {
  const res = await httpClient.get(`/api/v1/inventory/products/${productId}/availability`)
  return unwrapApiResponse(res.data)
}

export async function adjustStock(payload) {
  const res = await httpClient.post('/api/v1/inventory/adjustments', payload)
  return unwrapApiResponse(res.data)
}

export async function receiveStock(payload) {
  const res = await httpClient.post('/api/v1/inventory/receive', payload)
  return unwrapApiResponse(res.data)
}
