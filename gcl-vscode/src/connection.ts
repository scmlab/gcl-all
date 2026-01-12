// This module handles connections:

import * as vscode from "vscode";
import { LanguageClient,
	LanguageClientOptions,
	ProtocolNotificationType,
	ServerOptions,
	TransportKind } from "vscode-languageclient/node";
import { FileStateNotification, ErrorNotification } from "./data/ClientState";

let client: LanguageClient | undefined;

export async function stop() {
	if (client) {
		try {
			await client.stop();
		} finally {
			client = undefined;
		}
	}
}

export async function sendRequest<R>(method: string, param: any): Promise<R> {
	if (!client) throw new Error('Language client is not running');
	return client.sendRequest(method, param);
}


export async function start() {
	const gclConfig = vscode.workspace.getConfiguration("gcl-vscode");
	const gclPath = gclConfig.get<string>("gclPath") ?? "gcl";

	const serverOptions: ServerOptions = {
		// TODO: Temporarily enable logging in both run and debug modes
		run: { command: gclPath, args: [`--out=./gcl_server.log`], transport: TransportKind.stdio},
		debug: { command: gclPath , args: [`--out=./gcl_server.log`], transport: TransportKind.stdio }
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for `.gcl` documents
		documentSelector: [{ scheme: 'file', language: 'gcl' }],
		synchronize: {
			// Notify the server about file changes to '.gcl' files contained in the workspace
			fileEvents: vscode.workspace.createFileSystemWatcher('**/.gcl')
		}
	};

	// Use "gcl-vscode" as the client ID (matches extension ID for consistency, though not required).
	// This enables automatic trace configuration: vscode-languageclient will automatically read
	// the "gcl-vscode.trace.server" setting without requiring manual setTrace() calls.
	client = new LanguageClient ("gcl-vscode", "GCL LSP Server", serverOptions, clientOptions);
	await client.start();
}

export function onUpdateNotification(handler: (fileStateNotification: FileStateNotification) => void) {
	if (!client) throw new Error('Language client is not running');
	return client.onNotification(new ProtocolNotificationType<FileStateNotification, any>("gcl/update"), handler)
}

export function onErrorNotification(handler: (errorNotification: ErrorNotification) => void) {
	if (!client) throw new Error('Language client is not running');
	return client.onNotification(new ProtocolNotificationType<ErrorNotification, any>("gcl/error"), handler)
}
