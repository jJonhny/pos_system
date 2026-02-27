import axios from 'axios'
import { clearAuthSnapshot, getAccessToken } from '../auth/tokenStorage'

const baseURL = import.meta.env.VITE_API_BASE_URL || ''

export const httpClient = axios.create({
  baseURL,
  timeout: 20000
})

httpClient.interceptors.request.use((config) => {
  const token = getAccessToken()
  if (token) {
    config.headers.Authorization = `Bearer ${token}`
  }
  config.headers['X-Requested-With'] = 'XMLHttpRequest'
  return config
})

httpClient.interceptors.response.use(
  (response) => response,
  (error) => {
    const status = error?.response?.status
    if (status === 401) {
      clearAuthSnapshot()
    }
    const serverMessage =
      error?.response?.data?.message ||
      error?.response?.data?.error ||
      error?.response?.data?.details?.[0]
    error.message = serverMessage || error.message || 'Request failed.'
    throw error
  }
)
