import { httpClient } from '../httpClient'
import { unwrapApiResponse } from '../apiResult'

export async function listProducts(params = {}) {
  const res = await httpClient.get('/api/v1/products', { params })
  return unwrapApiResponse(res.data)
}

export async function getProduct(productId) {
  const res = await httpClient.get(`/api/v1/products/${productId}`)
  return unwrapApiResponse(res.data)
}

export async function createProduct(payload) {
  const res = await httpClient.post('/api/v1/products', payload)
  return unwrapApiResponse(res.data)
}

export async function updateProduct(productId, payload) {
  const res = await httpClient.put(`/api/v1/products/${productId}`, payload)
  return unwrapApiResponse(res.data)
}
