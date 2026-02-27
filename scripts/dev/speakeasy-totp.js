#!/usr/bin/env node
const speakeasy = require("speakeasy");
const QRCode = require("qrcode");

async function main() {
  const command = process.argv[2];

  if (command === "setup") {
    const label = process.argv[3];
    const issuer = process.argv[4] || "POS-System";
    if (!label) {
      throw new Error("Missing label for setup command.");
    }

    const secret = speakeasy.generateSecret({
      name: `${issuer}:${label}`,
      issuer,
      length: 20,
    });

    const qrDataUrl = await QRCode.toDataURL(secret.otpauth_url, {
      errorCorrectionLevel: "M",
      margin: 1,
      width: 240,
    });

    process.stdout.write(
      JSON.stringify({
        base32: secret.base32,
        otpauthUrl: secret.otpauth_url,
        qrDataUrl,
      })
    );
    return;
  }

  if (command === "verify") {
    const base32Secret = process.argv[3];
    const token = process.argv[4];
    const window = Number(process.argv[5] || "1");
    if (!base32Secret || !token) {
      throw new Error("Missing secret or token for verify command.");
    }

    const valid = speakeasy.totp.verify({
      secret: base32Secret,
      encoding: "base32",
      token,
      window,
    });

    process.stdout.write(JSON.stringify({ valid: Boolean(valid) }));
    return;
  }

  throw new Error("Unsupported command. Use 'setup' or 'verify'.");
}

main().catch((error) => {
  process.stderr.write(String(error && error.message ? error.message : error));
  process.exit(1);
});
