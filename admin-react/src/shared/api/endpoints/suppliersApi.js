import { httpClient } from '../httpClient'
import { unwrapApiResponse } from '../apiResult'

export async function listSuppliers(params = {}) {
  const res = await httpClient.get('/api/v1/suppliers', { params })
  return unwrapApiResponse(res.data)
}

export async function getSupplier(supplierId) {
  const res = await httpClient.get(`/api/v1/suppliers/${supplierId}`)
  return unwrapApiResponse(res.data)
}

export async function createSupplier(payload) {
  const res = await httpClient.post('/api/v1/suppliers', payload)
  return unwrapApiResponse(res.data)
}

export async function updateSupplier(supplierId, payload) {
  const res = await httpClient.put(`/api/v1/suppliers/${supplierId}`, payload)
  return unwrapApiResponse(res.data)
}

export async function deleteSupplier(supplierId) {
  const res = await httpClient.delete(`/api/v1/suppliers/${supplierId}`)
  return unwrapApiResponse(res.data)
}
