export function Spinner({ label = 'Loading...' }) {
  return (
    <div className="state-box">
      <div className="spinner" />
      <p>{label}</p>
    </div>
  )
}
