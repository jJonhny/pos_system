import { createBrowserRouter, Navigate } from 'react-router-dom'
import { ProtectedRoute } from './routes/ProtectedRoute'
import { AdminLayout } from './layouts/AdminLayout'
import { LoginPage } from '../features/auth/pages/LoginPage'
import { DashboardPage } from '../pages/DashboardPage'
import { UsersPage } from '../pages/UsersPage'
import { ProductsPage } from '../pages/ProductsPage'
import { InventoryPage } from '../pages/InventoryPage'
import { ReportsPage } from '../pages/ReportsPage'
import { AuditLogsPage } from '../pages/AuditLogsPage'
import { NotFoundPage } from '../pages/NotFoundPage'
import { UnauthorizedPage } from '../pages/UnauthorizedPage'
import { ADMIN_ROLES, MANAGEMENT_ROLES } from '../shared/auth/roles'

export const router = createBrowserRouter([
  {
    path: '/',
    element: <Navigate to="/admin" replace />
  },
  {
    path: '/login',
    element: <LoginPage />
  },
  {
    path: '/unauthorized',
    element: <UnauthorizedPage />
  },
  {
    path: '/admin',
    element: <ProtectedRoute />,
    children: [
      {
        element: <AdminLayout />,
        children: [
          {
            index: true,
            element: <DashboardPage />
          },
          {
            element: <ProtectedRoute allowedRoles={ADMIN_ROLES} />,
            children: [
              {
                path: 'users',
                element: <UsersPage />
              },
              {
                path: 'audit',
                element: <AuditLogsPage />
              }
            ]
          },
          {
            element: <ProtectedRoute allowedRoles={MANAGEMENT_ROLES} />,
            children: [
              {
                path: 'products',
                element: <ProductsPage />
              },
              {
                path: 'inventory',
                element: <InventoryPage />
              },
              {
                path: 'reports',
                element: <ReportsPage />
              }
            ]
          }
        ]
      }
    ]
  },
  {
    path: '*',
    element: <NotFoundPage />
  }
])
