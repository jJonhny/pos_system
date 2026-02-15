# POS Bridge (Node)

Local companion service for cashier machines.

## What it does

- Exposes localhost-only endpoints:
  - `POST /print`
  - `POST /drawer/open`
  - `GET /health`
- Sends ESC/POS text payloads to configured printers (network or USB).
- Supports printer selection by `terminalId`.

## Setup

1. Copy `config.example.json` to `config.json`.
2. Update printer configuration for each terminal.
3. Install dependencies:
   - `npm install`
4. Start bridge:
   - `npm start`

Default URL: `http://127.0.0.1:18765`

## Example config

```json
{
  "host": "127.0.0.1",
  "port": 18765,
  "defaultTerminalId": "TERM-01",
  "allowedOrigins": [
    "http://127.0.0.1:8080",
    "http://localhost:8080"
  ],
  "terminals": {
    "TERM-01": {
      "type": "network",
      "host": "192.168.1.60",
      "port": 9100
    }
  }
}
```

## API

### `POST /print`

Body:

```json
{
  "terminalId": "TERM-01",
  "text": "Receipt lines...",
  "encoding": "CP437",
  "cut": true,
  "drawerPulse": false,
  "qrPayload": "sale=123|..."
}
```

### `POST /drawer/open`

Body:

```json
{
  "terminalId": "TERM-01"
}
```

## Notes

- Keep this service bound to `127.0.0.1` only.
- Run one bridge process per cashier machine.
- For network printers, ensure the POS machine can reach printer port `9100`.
