// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { retrieveMainEditor } from './utils'
import { start, stop, sendRequest, onUpdateNotification, onErrorNotification } from "./connection";
import { GclPanel } from './gclPanel';
import { ISpecification } from './data/FileState';
import { ClientState } from './data/ClientState';
import path from 'path';


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


	// 切換 tab 到 .gcl 檔的時候，將該檔案對應的狀態顯示在 gclPanel
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

	// notification gcl/update
	// 更新 clientState 裡的 specs, pos, warnings
	const updateNotificationHandlerDisposable = onUpdateNotification(async ({
		filePath,
		specs,
		pos,
		warnings
	}) => {
		vscode.window.showErrorMessage(JSON.stringify({specs}))
		const oldClientState: ClientState | undefined = context.workspaceState.get(filePath);
		let newClientState: ClientState =
			oldClientState
			? {specs, pos, warnings, errors: oldClientState.errors}
			: {specs, pos, warnings, errors: []};
		await context.workspaceState.update(filePath, newClientState);
		gclPanel.rerender(newClientState);
		await updateInlayHints(newClientState);

		async function updateInlayHints(newClientState: ClientState) {
			// TODO: find a way to tell vscode to update inlay hints
		}
	});
	context.subscriptions.push(updateNotificationHandlerDisposable);

	// notification gcl/error
	// 更新 clientState 裡的 errors
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
}

export async function deactivate() {
	console.log('deactivating gcl-vscode');
	try {
		await stop();
	} catch (e:any) {
		console.error('Error stopping client', e);
	}
}
