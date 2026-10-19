// This module handles connections:

import * as vscode from "vscode";
import * as fs from "fs";
import * as path from "path";
import {
  LanguageClient,
  LanguageClientOptions,
  ProtocolNotificationType,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";
import { FileStateNotification } from "./data/ClientState";

let client: LanguageClient | undefined;
let extensionContext: vscode.ExtensionContext | undefined;

export async function stop() {
  if (client && client.needsStop()) {
    await client.stop();
  }
}

// Stop (best-effort) and start the server again on the same LanguageClient
// instance. Reusing the instance matters: vscode-languageclient keeps
// notification handlers (e.g. our gcl/update handler) on the client and
// re-installs them on the new connection, so creating a fresh client here
// would silently drop them.
export async function restart() {
  try {
    await stop();
  } catch (e) {
    // The server may have crashed or hung; start a fresh one anyway.
    console.error("Error stopping GCL server during restart:", e);
  }
  await start();
}

export async function sendRequest<R>(method: string, param: any): Promise<R> {
  if (!client) throw new Error("Language client is not running");
  return client.sendRequest(method, param);
}

/**
 * Resolves the path to the `gcl` LSP executable:
 *   1. An explicit `gcl-vscode.gclPath` setting wins (power users / dev builds).
 *   2. Otherwise the binary bundled in the platform-specific VSIX (bin/gcl).
 *   3. Otherwise fall back to `gcl` on PATH.
 */
function resolveGclPath(context: vscode.ExtensionContext | undefined): string {
  const configured = vscode.workspace
    .getConfiguration("gcl-vscode")
    .get<string>("gclPath")
    ?.trim();
  if (configured) return configured;

  if (context) {
    const bundled = path.join(
      context.extensionPath,
      "bin",
      process.platform === "win32" ? "gcl.exe" : "gcl",
    );
    if (fs.existsSync(bundled)) {
      // Packaging into a VSIX can drop the executable bit; restore it.
      if (process.platform !== "win32") {
        try {
          fs.chmodSync(bundled, 0o755);
        } catch {
          // best-effort: a spawn error later will surface the real problem
        }
      }
      return bundled;
    }
  }

  return "gcl";
}

/**
 * Builds the environment for the spawned `gcl` process so it can find the
 * `z3` solver bundled alongside the binary in the VSIX's `bin/` directory.
 * gcl reaches z3 through sbv, which by default looks up `z3` on PATH, so we
 * unconditionally prepend bin/ to PATH. This is harmless when no bundled z3
 * is present (dev builds via gclPath): the lookup simply falls through to the
 * rest of PATH.
 */
function gclProcessEnv(): NodeJS.ProcessEnv | undefined {
  if (!extensionContext) return undefined;

  const binDir = path.join(extensionContext.extensionPath, "bin");

  // Packaging into a VSIX can drop the executable bit on the bundled z3;
  // restore it (best-effort — a spawn error later would surface a real issue).
  if (process.platform !== "win32") {
    try {
      fs.chmodSync(path.join(binDir, "z3"), 0o755);
    } catch {
      // no bundled z3 (or already executable); nothing to do
    }
  }

  // Update PATH under its existing key casing (Windows commonly uses `Path`);
  // spreading process.env into a plain object loses the case-insensitive
  // lookup, so blindly setting `env.PATH` could add a duplicate key that the
  // child process ignores.
  const env = { ...process.env };
  const pathKey =
    Object.keys(env).find((k) => k.toLowerCase() === "path") ?? "PATH";
  env[pathKey] = binDir + path.delimiter + (env[pathKey] ?? "");
  return env;
}

function createClient(): LanguageClient {
  const gclPath = resolveGclPath(extensionContext);
  const env = gclProcessEnv();

  const serverOptions: ServerOptions = {
    // The server logs to stderr in both run and debug modes.
    run: {
      command: gclPath,
      options: { env },
      transport: TransportKind.stdio,
    },
    debug: {
      command: gclPath,
      options: { env },
      transport: TransportKind.stdio,
    },
  };

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    // Register the server for `.gcl` documents
    documentSelector: [{ scheme: "file", language: "gcl" }],
    synchronize: {
      // Notify the server about file changes to '.gcl' files contained in the workspace
      fileEvents: vscode.workspace.createFileSystemWatcher("**/.gcl"),
    },
  };

  // Use "gcl-vscode" as the client ID (matches extension ID for consistency, though not required).
  // This enables automatic trace configuration: vscode-languageclient will automatically read
  // the "gcl-vscode.trace.server" setting without requiring manual setTrace() calls.
  return new LanguageClient(
    "gcl-vscode",
    "GCL LSP Server",
    serverOptions,
    clientOptions,
  );
}

export async function start(context?: vscode.ExtensionContext) {
  if (context) extensionContext = context;
  if (!client) client = createClient();
  await client.start();
}

export function onFileStateNotification(
  handler: (fileStateNotification: FileStateNotification) => void,
) {
  if (!client) throw new Error("Language client is not running");
  return client.onNotification(
    new ProtocolNotificationType<FileStateNotification, any>("gcl/update"),
    handler,
  );
}
