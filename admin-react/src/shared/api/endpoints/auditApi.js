import { httpClient } from '../httpClient'
import { unwrapApiResponse } from '../apiResult'

export async function auditFilterMeta() {
  const res = await httpClient.get('/api/v1/audit/meta')
  return unwrapApiResponse(res.data)
}

export async function auditEvents(params = {}) {
  const res = await httpClient.get('/api/v1/audit/events', { params })
  return unwrapApiResponse(res.data)
}
