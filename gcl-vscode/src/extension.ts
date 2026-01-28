// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { TextDocumentEdit } from 'vscode-languageclient';
import { start, stop, sendRequest, onUpdateNotification, onErrorNotification, start2 } from "./connection";
import { GclPanel } from './gclPanel';
import { ISpecification } from './data/FileState';
import { ClientState } from './data/ClientState';

// Load response type - uses LSP's TextDocumentEdit
type LoadResponse =
	| { status: "done" }
	| { status: "needsEdit"; textDocumentEdit: TextDocumentEdit };

// Load function that handles needsEdit response recursively
async function load(filePath: string): Promise<void> {
	const response = await sendRequest<LoadResponse>("gcl/reload", { filePath });

	if (response.status === "done") {
		return;
	}

	if (response.status === "needsEdit") {
		const { textDocumentEdit } = response;
		const uri = vscode.Uri.parse(textDocumentEdit.textDocument.uri);

		// Version check before applying edit
		const doc = vscode.workspace.textDocuments.find(d => d.uri.toString() === uri.toString());
		if (doc && doc.version !== textDocumentEdit.textDocument.version) {
			vscode.window.showInformationMessage(`Version mismatch (doc=${doc.version}, edit=${textDocumentEdit.textDocument.version}), retrying...`);
			await load(filePath);
			return;
		}

		// Convert to vscode WorkspaceEdit
		const workspaceEdit = new vscode.WorkspaceEdit();
		for (const edit of textDocumentEdit.edits) {
			const range = new vscode.Range(
				new vscode.Position(edit.range.start.line, edit.range.start.character),
				new vscode.Position(edit.range.end.line, edit.range.end.character)
			);
			workspaceEdit.replace(uri, range, edit.newText);
		}

		const success = await vscode.workspace.applyEdit(workspaceEdit);
		if (!success) {
			vscode.window.showInformationMessage("Apply edit failed, retrying...");
			await load(filePath);
			return;
		}

		// Apply succeeded, wait for didChange to be sent, then recursively call load.
		// This sleep may not be strictly necessary, but is added as a precaution to ensure
		// the server receives the didChange notification before the next reload request.
		// See _pendingChangeDelayer in vscode-languageserver-node for reference.
		await new Promise(resolve => setTimeout(resolve, 260));
		await load(filePath);
	}
}


export async function activate(context: vscode.ExtensionContext) {
	console.log('activating gcl-vscode');


	// Displays pre- and post- conditions as inline hints around specs
	// TODO: Fully display long inlay hints.
	// ^^^^^ P.S. This doesn't seem to be solvable with the current VSCode version. We have to wait.
	const inlayHintsDisposable = vscode.languages.registerInlayHintsProvider(
		{ scheme: 'file', language: 'gcl' },
		{
			provideInlayHints(document, visableRange, token): vscode.InlayHint[] {
				let filePath: string = document.uri.fsPath
				const clientState: ClientState | undefined = context.workspaceState.get(filePath);
				const specs: ISpecification[] = clientState? clientState.specs : [];

				const inlayHints = specs.flatMap((spec: ISpecification) => {
					let start = new vscode.Position(spec.specRange.start.line, spec.specRange.start.character);
					let end = new vscode.Position(spec.specRange.end.line, spec.specRange.end.character);
					if (visableRange.contains(start) || visableRange.contains(end)) {
						const preConditionHint = new vscode.InlayHint(start.translate(0, 2), `${spec.preCondition}`);
						preConditionHint.paddingLeft = true;
						const postConditionHint = new vscode.InlayHint(end, `${spec.postCondition}`);
						postConditionHint.paddingLeft = true
						return [ preConditionHint, postConditionHint ];
					}
					return [];
				});
				return inlayHints;
			}
		}
	)
	context.subscriptions.push(inlayHintsDisposable);
	

	const gclPanel = new GclPanel();
	gclPanel.showLoading(context.extensionPath);
	// We prevent focusing on the panel instead of the text editor.
	vscode.commands.executeCommand('workbench.action.focusFirstEditorGroup');


	// When switching tabs to a .gcl file, display the corresponding state in gclPanel
	const changeTabDisposable = vscode.window.tabGroups.onDidChangeTabs((event: vscode.TabChangeEvent) => {
		const changedTab: vscode.Tab = event.changed[0]
		const isFileTab: boolean = "uri" in (changedTab.input as any);
		if (isFileTab) {
			const filePath = (changedTab.input as {uri: vscode.Uri}).uri.fsPath;
			let clientState: ClientState | undefined = context.workspaceState.get(filePath);
			if (clientState) gclPanel.rerender(clientState);
		}
	});
	context.subscriptions.push(changeTabDisposable);

	// request gcl/reload
	const reloadDisposable = vscode.commands.registerCommand('gcl.reload', async () => {
		const document = vscode.window.activeTextEditor?.document;
		if (document?.uri.scheme === 'file' && document.languageId === 'gcl') {
			await load(document.uri.fsPath);
		}
	});
	context.subscriptions.push(reloadDisposable);

	// refine gcl/refine
	const refineDisposable = vscode.commands.registerCommand('gcl.refine', async () => {
		const editor = vscode.window.activeTextEditor;
		const document = editor?.document;
		if (editor && document?.uri.scheme === 'file' && document.languageId === 'gcl') {
			await sendRequest("gcl/refine", {
				filePath: document.uri.fsPath,
				line: editor.selection.start.line, // 0-based
				character: editor.selection.start.character, // 0-based
			});
		}
	});
	context.subscriptions.push(refineDisposable);

	await start();

	const outputChannel = vscode.window.createOutputChannel("GCL");
	context.subscriptions.push(outputChannel);

	// notification gcl/update
	// Update specs, pos, warnings in clientState, and clear errors
	const updateNotificationHandlerDisposable = onUpdateNotification(async ({
		filePath,
		specs,
		pos,
		warnings
	}) => {
		const timestamp = new Date().toLocaleString();
		outputChannel.appendLine(`[${timestamp}] Received update for ${filePath}:`);
		outputChannel.appendLine(JSON.stringify({ specs }, null, 2));

		// Clear errors when receiving a successful update
		let newClientState: ClientState = { specs, pos, warnings, errors: [] };

		await context.workspaceState.update(filePath, newClientState);
		gclPanel.rerender(newClientState);
		await updateInlayHints(newClientState);

		async function updateInlayHints(newClientState: ClientState) {
			// TODO: find a way to tell vscode to update inlay hints
		}
	});
	context.subscriptions.push(updateNotificationHandlerDisposable);

	// notification gcl/error
	// Update errors in clientState
	const errorNotificationHandlerDisposable = onErrorNotification(async ({
		filePath,
		errors
	}) => {
		const oldClientState: ClientState | undefined = context.workspaceState.get(filePath);
		const newClientState: ClientState =
			oldClientState
			? {errors, specs: oldClientState.specs, pos: oldClientState.pos, warnings: oldClientState.warnings}
			: {errors, specs: [], pos: [], warnings: []};
		await context.workspaceState.update(filePath, newClientState);
		gclPanel.rerender(newClientState);
	});
	context.subscriptions.push(errorNotificationHandlerDisposable);

	// Trigger load when a GCL file is opened
	const didOpenDisposable = vscode.workspace.onDidOpenTextDocument(async (document) => {
		outputChannel.appendLine("[Trace] client side: onDidOpenTextDocument: " + document.uri);
		if (document.uri.scheme === 'file' && document.languageId === 'gcl') {
			const filePath = document.uri.fsPath;
			outputChannel.appendLine("[Trace] client side: onDidOpenTextDocument: calling load");

			setTimeout(async () => {
				await load(filePath);
			}, 10);
		}
	});
	context.subscriptions.push(didOpenDisposable);

	// Trigger load when a GCL file is saved
	const didSaveDisposable = vscode.workspace.onDidSaveTextDocument(async (document) => {
		if (document.uri.scheme === 'file' && document.languageId === 'gcl') {
			const filePath = document.uri.fsPath;
			await load(filePath);
		}
	});
	context.subscriptions.push(didSaveDisposable);

	// restart server command
	const restartDisposable = vscode.commands.registerCommand('gcl.restartServer', async () => {
		try {
			await stop();
			await start();
			vscode.window.showInformationMessage('GCL server restarted');
		} catch (e:any) {
			vscode.window.showErrorMessage('Failed to restart GCL server: ' + e.message);
		}
	});
	context.subscriptions.push(restartDisposable);

	// request gcl/debug
	const debugDisposable = vscode.commands.registerCommand('gcl.debug', async () => {
		const document = vscode.window.activeTextEditor?.document;
		if (document?.uri.scheme === 'file' && document.languageId === 'gcl') {
			await sendRequest("gcl/debug", { filePath: document.uri.fsPath });
		}
	});
	context.subscriptions.push(debugDisposable);

	await start2();

	// Also trigger load for any GCL files that are already open
	for (const document of vscode.workspace.textDocuments) {
		if (document.uri.scheme === 'file' && document.languageId === 'gcl') {
			const filePath = document.uri.fsPath;
			await load(filePath);
		}
	}
}

export async function deactivate() {
	console.log('deactivating gcl-vscode');
	try {
		await stop();
	} catch (e:any) {
		console.error('Error stopping client', e);
	}
}
