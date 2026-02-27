import { httpClient } from '../httpClient'
import { unwrapApiResponse } from '../apiResult'

export async function reportSummary(params = {}) {
  const res = await httpClient.get('/api/v1/reports/summary', { params })
  return unwrapApiResponse(res.data)
}

export async function reportSales(params = {}) {
  const res = await httpClient.get('/api/v1/reports/sales', { params })
  return unwrapApiResponse(res.data)
}

export async function reportShifts(params = {}) {
  const res = await httpClient.get('/api/v1/reports/shifts', { params })
  return unwrapApiResponse(res.data)
}
