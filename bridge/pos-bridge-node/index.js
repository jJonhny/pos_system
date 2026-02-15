const fs = require("fs");
const path = require("path");
const express = require("express");
const cors = require("cors");
const escpos = require("escpos");

escpos.USB = require("escpos-usb");
escpos.Network = require("escpos-network");

const DEFAULT_CONFIG = {
  host: "127.0.0.1",
  port: 18765,
  defaultTerminalId: "TERM-01",
  allowedOrigins: ["http://127.0.0.1:8080", "http://localhost:8080"],
  terminals: {}
};

function loadConfig() {
  const explicit = process.env.POS_BRIDGE_CONFIG;
  const configPath = explicit
    ? path.resolve(explicit)
    : path.join(__dirname, "config.json");

  if (!fs.existsSync(configPath)) {
    return { ...DEFAULT_CONFIG };
  }

  const parsed = JSON.parse(fs.readFileSync(configPath, "utf8"));
  return {
    ...DEFAULT_CONFIG,
    ...parsed,
    terminals: parsed.terminals || {}
  };
}

const config = loadConfig();

function normalizeTerminalId(value) {
  if (!value || typeof value !== "string") return null;
  const cleaned = value.trim().toUpperCase();
  if (!cleaned) return null;
  return cleaned.length <= 128 ? cleaned : cleaned.slice(0, 128);
}

function resolveTerminal(terminalId) {
  const normalized = normalizeTerminalId(terminalId) || normalizeTerminalId(config.defaultTerminalId);
  if (!normalized) {
    throw new Error("Terminal ID is required.");
  }
  const terminal = config.terminals[normalized];
  if (!terminal) {
    throw new Error(`No printer configured for terminal ${normalized}.`);
  }
  return { terminalId: normalized, printer: terminal };
}

function createDevice(printer) {
  const type = String(printer.type || "").toLowerCase();
  if (type === "network") {
    const host = printer.host;
    const port = Number(printer.port || 9100);
    if (!host) throw new Error("Network printer host is required.");
    return new escpos.Network(host, port);
  }
  if (type === "usb") {
    const vendorId = Number(printer.vendorId || printer.vendorID || 0);
    const productId = Number(printer.productId || printer.productID || 0);
    if (!vendorId || !productId) {
      throw new Error("USB printer vendorId/productId are required.");
    }
    return new escpos.USB(vendorId, productId);
  }
  throw new Error(`Unsupported printer type: ${printer.type}`);
}

function withPrinter(terminalId, encoding, work) {
  const { terminalId: resolvedTerminal, printer } = resolveTerminal(terminalId);
  const device = createDevice(printer);

  return new Promise((resolve, reject) => {
    device.open((error) => {
      if (error) {
        reject(error);
        return;
      }

      try {
        const printerClient = new escpos.Printer(device, { encoding: encoding || "CP437" });
        work(printerClient, resolvedTerminal);
        setTimeout(() => resolve(resolvedTerminal), 180);
      } catch (err) {
        reject(err);
      }
    });
  });
}

function sanitizeText(value) {
  if (value == null) return "";
  return String(value).replace(/\r\n/g, "\n");
}

const app = express();

app.use(express.json({ limit: "1mb" }));
app.use(cors({
  origin(origin, callback) {
    if (!origin) return callback(null, true);
    if (config.allowedOrigins.includes(origin)) return callback(null, true);
    return callback(new Error(`Origin ${origin} is not allowed`));
  },
  methods: ["GET", "POST"]
}));

app.get("/health", (_, res) => {
  res.json({ ok: true, bridge: "pos-bridge-node", time: new Date().toISOString() });
});

app.post("/print", async (req, res) => {
  const payload = req.body || {};
  const terminalId = normalizeTerminalId(payload.terminalId);
  const encoding = payload.encoding || "CP437";
  const cut = payload.cut !== false;
  const text = sanitizeText(payload.text);
  const rawBase64 = payload.rawBase64;

  try {
    const usedTerminal = await withPrinter(terminalId, encoding, (printer) => {
      if (rawBase64) {
        const raw = Buffer.from(rawBase64, "base64");
        printer.raw(raw);
      } else {
        text.split("\n").forEach((line) => printer.text(line));
      }
      if (payload.qrPayload) {
        const qrText = sanitizeText(payload.qrPayload);
        if (qrText) {
          printer.text(" ");
          printer.text(`QR: ${qrText}`);
        }
      }
      if (payload.drawerPulse) {
        printer.raw(Buffer.from([0x1b, 0x70, 0x00, 0x19, 0xfa]));
      }
      if (cut) printer.cut();
      printer.close();
    });

    res.json({ ok: true, terminalId: usedTerminal });
  } catch (error) {
    res.status(500).json({ ok: false, message: error.message || "Print failed." });
  }
});

app.post("/drawer/open", async (req, res) => {
  const payload = req.body || {};
  const terminalId = normalizeTerminalId(payload.terminalId);

  try {
    const usedTerminal = await withPrinter(terminalId, "CP437", (printer) => {
      printer.raw(Buffer.from([0x1b, 0x70, 0x00, 0x19, 0xfa]));
      printer.close();
    });

    res.json({ ok: true, terminalId: usedTerminal });
  } catch (error) {
    res.status(500).json({ ok: false, message: error.message || "Drawer open failed." });
  }
});

app.listen(config.port, config.host, () => {
  process.stdout.write(`POS Bridge running at http://${config.host}:${config.port}\n`);
});
