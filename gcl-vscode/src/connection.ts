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
	const gclConfig = vscode.workspace.getConfiguration("gcl-vscode");
	const gclPath = gclConfig.get<string>("gclPath") ?? "gcl";

	const serverOptions: ServerOptions = {
		run: { command: gclPath, transport: TransportKind.stdio},
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

	client = new LanguageClient ("GCL", "GCL LSP Server", serverOptions, clientOptions);
	await client.start();
}

export function onUpdateNotification(handler: (fileState: FileState) => void) {
	return client.onNotification(new ProtocolNotificationType<FileState, any>("gcl/update"), handler)
}

export function onErrorNotification(handler: (fileState: FileState) => void) {
	return client.onNotification(new ProtocolNotificationType<FileState, any>("gcl/error"), handler)
}
