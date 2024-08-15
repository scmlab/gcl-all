// This module handles connections:

import * as vscode from "vscode";
import { LanguageClient,
	LanguageClientOptions,
	ProtocolNotificationType,
	ServerOptions,
	TransportKind } from "vscode-languageclient/node";
import { FileState } from "./data/FileState";

let client: LanguageClient;

export function stop() {
	client.stop()
}

export async function sendRequest<R>(method: string, param: any): Promise<R> {
	return client.sendRequest(method, param);
}


export async function start() {
	const serverOptions: ServerOptions = {
		run: { command: "/Users/vince/.local/bin/gcl", transport: TransportKind.stdio },
		debug: { command: "/Users/vince/.local/bin/gcl" , transport: TransportKind.stdio }
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for 'guabao' documents
		documentSelector: [{ scheme: 'file', language: 'guabao' }],
		synchronize: {
			// Notify the server about file changes to '.gcl' files contained in the workspace
			fileEvents: vscode.workspace.createFileSystemWatcher('**/.gcl')
		}
	};

	client = new LanguageClient ("GBM", "GuaBao LSP Server", serverOptions, clientOptions);
	await client.start();
	return client
}

export function onUpdateNotification(handler: (fileState: FileState) => void) {
	return client.onNotification(new ProtocolNotificationType<FileState, any>("guabao/update"), handler)
}

export function onErrorNotification(handler: (fileState: FileState) => void) {
	return client.onNotification(new ProtocolNotificationType<FileState, any>("guabao/error"), handler)
}
