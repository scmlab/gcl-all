// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { retrieveMainEditor } from './utils'
import { start, stop, sendRequest, onFileStateNotification } from "./connection";
import { GclPanel } from './gclPanel';
import { IHole, ISpecification, ClientFileState } from './data/FileState';
import path from 'path';


export async function activate(context: vscode.ExtensionContext) {
	console.log('activating gcl-vscode');

	// Persists for the extension lifetime (activate is called once per session).
	// Keyed by filePath; entries are added/updated on gcl/update notifications.
	const fileStateMap = new Map<string, ClientFileState>();


	// Displays pre- and post- conditions as inline hints around specs
	// TODO: Fully display long inlay hints.
	// ^^^^^ P.S. This doesn't seem to be solvable with the current VSCode version. We have to wait.
	const inlayHintsEmitter = new vscode.EventEmitter<void>();
	const inlayHintsDisposable = vscode.languages.registerInlayHintsProvider(
		{ scheme: 'file', language: 'gcl' },
		{
			onDidChangeInlayHints: inlayHintsEmitter.event,
			provideInlayHints(document, visableRange, token): vscode.InlayHint[] {
				let filePath: string = document.uri.fsPath
				const clientState: ClientFileState | undefined = fileStateMap.get(filePath);

				if (clientState === undefined)
					return [];

				const inlayHints = clientState.specs.flatMap((spec: ISpecification) => {
					const start = new vscode.Position(spec.specRange.start.line, spec.specRange.start.character);
					const end = new vscode.Position(spec.specRange.end.line, spec.specRange.end.character);
					if (visableRange.contains(start) || visableRange.contains(end)) {
						const preConditionHint = new vscode.InlayHint(start.translate(0, 2), `${spec.preCondition}`);
						preConditionHint.paddingLeft = true;
						const postConditionHint = new vscode.InlayHint(end, `${spec.postCondition}`);
						postConditionHint.paddingLeft = true
						return [ preConditionHint, postConditionHint ];
					}
					return [];
				});

				inlayHints.push(...clientState.holes.flatMap((hole: IHole) => {
					const holeRange = hole.holeRange;
					const start = new vscode.Position(holeRange.start.line, holeRange.start.character);
					const end = new vscode.Position(holeRange.end.line, holeRange.end.character);
					if (visableRange.contains(start) || visableRange.contains(end)) {
						const preConditionHint = new vscode.InlayHint(end.translate(0, -2), `${hole.holeID}`);
						preConditionHint.paddingLeft = true;
						return [ preConditionHint ];
					}
					
					return [];
				}));

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
			let clientState: ClientFileState | undefined = fileStateMap.get(filePath);
			if (clientState) gclPanel.rerender(clientState);
		}
	});
	context.subscriptions.push(changeTabDisposable);

	const closeDocDisposable = vscode.workspace.onDidCloseTextDocument((document) => {
		fileStateMap.delete(document.uri.fsPath);
	});
	context.subscriptions.push(closeDocDisposable);

	// request gcl/reload
	const reloadDisposable = vscode.commands.registerCommand('gcl.reload', async () => {
		const editor = retrieveMainEditor();
		// Get the path for the current text file.
		const filePath = editor?.document.uri.fsPath;
		// Send the request asynchronously.
		const _response =  await sendRequest("gcl/reload", {filePath: filePath})
		// ignore the response and get results or errors from notifications
	});
	context.subscriptions.push(reloadDisposable);

	// refine gcl/refine
	const refineDisposable = vscode.commands.registerCommand('gcl.refine', async () => {
		const editor = retrieveMainEditor();
		const filePath = editor.document.uri.fsPath;
		const _response = await sendRequest("gcl/refine", {
			filePath: filePath,
			line: editor.selection.start.line, // 0-based
			character: editor.selection.start.character, // 0-based
		})
		// ignore the response and get results or errors from notifications
	});
	context.subscriptions.push(refineDisposable);

	await start();

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
		const editor = retrieveMainEditor();
		const filePath = editor?.document.uri.fsPath;
		const _response = await sendRequest("gcl/debug", {filePath: filePath})
	});
	context.subscriptions.push(debugDisposable);

	const outputChannel = vscode.window.createOutputChannel("GCL");
	context.subscriptions.push(outputChannel);

	// notification gcl/update
	const updateNotificationHandlerDisposable = onFileStateNotification(async ({
		filePath,
		errors,
		holes,
		specs,
		pos,
		warnings
	}) => {
		const timestamp = new Date().toLocaleString();
		outputChannel.appendLine(`[${timestamp}] Received update for ${filePath}:`);
		outputChannel.appendLine(JSON.stringify({ specs }, null, 2));

		let newClientFileState: ClientFileState = { errors, holes, specs, pos, warnings };

		fileStateMap.set(filePath, newClientFileState);
		gclPanel.rerender(newClientFileState);
		await updateInlayHints(newClientFileState);

		async function updateInlayHints(newClientFileState: ClientFileState) {
			inlayHintsEmitter.fire();
		}
	});
	context.subscriptions.push(updateNotificationHandlerDisposable);
}

export async function deactivate() {
	console.log('deactivating gcl-vscode');
	try {
		await stop();
	} catch (e:any) {
		console.error('Error stopping client', e);
	}
}
