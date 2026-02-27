import { httpClient } from '../httpClient'
import { unwrapApiResponse } from '../apiResult'

export async function listUsers(params = {}) {
  const res = await httpClient.get('/api/v1/users', { params })
  return unwrapApiResponse(res.data)
}

export async function getUser(userId) {
  const res = await httpClient.get(`/api/v1/users/${userId}`)
  return unwrapApiResponse(res.data)
}

export async function createUser(payload) {
  const res = await httpClient.post('/api/v1/users', payload)
  return unwrapApiResponse(res.data)
}

export async function updateUserRole(userId, role) {
  const res = await httpClient.patch(`/api/v1/users/${userId}/role`, { role })
  return unwrapApiResponse(res.data)
}

export async function updateUserStatus(userId, active) {
  const res = await httpClient.patch(`/api/v1/users/${userId}/status`, { active })
  return unwrapApiResponse(res.data)
}

export async function updateUserPermissions(userId, permissions) {
  const res = await httpClient.put(`/api/v1/users/${userId}/permissions`, { permissions })
  return unwrapApiResponse(res.data)
}
