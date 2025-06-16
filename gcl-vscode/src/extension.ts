// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { retrieveMainEditor, genSelectionRangeWithOffset } from './utils'
import { start, stop, sendRequest, onUpdateNotification, onErrorNotification } from "./connection";
import { getSpecLinesRange, getImplText, getSpecText, getImplLinesRange } from "./refine";
import { GclPanel } from './gclPanel';
import { FileState, ISpecification } from './data/FileState';
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
				const fileState: FileState | undefined = context.workspaceState.get(filePath);
				const specs: ISpecification[] = fileState? fileState.specs : [];

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
			let fileState: FileState | undefined = context.workspaceState.get(filePath);
			if (fileState) gclPanel.rerender(fileState);
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
		const selectionRange = genSelectionRangeWithOffset(editor);
		let specLines = getSpecLinesRange(editor, selectionRange);
		
		if (specLines) {
			const implText = getImplText(editor, specLines);
			const specText = getSpecText(editor, specLines);
			const implLines = getImplLinesRange(editor, specLines);
			const _response = await sendRequest("gcl/refine", {
				filePath: filePath,
				implStart: implLines.toJson().start,
				specText,
				specLines: specLines.toJson(),
			})
			// ignore the response and get results or errors from notifications
		} else {
			vscode.window.showWarningMessage("Please place the cursor inside the specification to refine.");
		}
	});
	context.subscriptions.push(refineDisposable);

	await start();

	// notification gcl/update
	// 更新 fileState 裡的 specs, pos, warnings
	const updateNotificationHandlerDisposable = onUpdateNotification(async ({
		filePath,
		specs,
		pos,
		warnings
	}) => {
		vscode.window.showErrorMessage(JSON.stringify({specs}))
		const oldFileState: FileState | undefined = context.workspaceState.get(filePath);
		let newFileState: FileState =
			oldFileState
			? {...oldFileState, specs, pos, warnings}
			: {filePath, specs, pos, warnings, errors: []};
		await context.workspaceState.update(filePath, newFileState);
		gclPanel.rerender(newFileState);
		await updateInlayHints(newFileState);

		async function updateInlayHints(newFileState: FileState) {
			// TODO: find a way to tell vscode to update inlay hints
		}
	});
	context.subscriptions.push(updateNotificationHandlerDisposable);

	// notification gcl/error
	// 更新 fileState 裡的 errors
	const errorNotificationHandlerDisposable = onErrorNotification(async ({
		filePath,
		errors
	}) => {
		const oldFileState: FileState | undefined = context.workspaceState.get(filePath);
		const newFileState: FileState =
			oldFileState
			? {...oldFileState, errors}
			: {filePath, specs: [], pos: [], warnings: [], errors};
		await context.workspaceState.update(filePath, newFileState);
		gclPanel.rerender(newFileState);
	});
	context.subscriptions.push(errorNotificationHandlerDisposable);
}

export function deactivate() {
	console.log('deactivating gcl-vscode');
	stop()
}
