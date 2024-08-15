// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { retrieveMainEditor, genSelectionRangeWithOffset, isGuabaoLabel } from './utils'
import { start, stop, sendRequest, onUpdateNotification, onErrorNotification } from "./connection";
import { getSpecRange, getSpecLinesRange, specContent, getImplText } from "./refine";
import { PanelProvider } from './gclPanel';
import { FileState, ISpecification } from './data/FileState';


export async function activate(context: vscode.ExtensionContext) {
	console.log('GuaBao VLang Mode is now active!');
	const panelProvider = new PanelProvider();


	// Displays pre- and post- conditions as inline hints around specs
	// TODO: Fully display long inlay hints.
	// ^^^^^ P.S. This doesn't seem to be solvable with the current VSCode version. We have to wait.
	// TODO: Do not display inlay artifacts.
	vscode.languages.registerInlayHintsProvider(
		{ scheme: 'file', language: 'guabao' },
		{
			provideInlayHints(document, range, token): vscode.InlayHint[] {
				let filePath: string = document.uri.fsPath
				// We check the editor in the state is what we really want. Else, do nothing.
				if (context.workspaceState.get("editor") === retrieveMainEditor()) {
					const fileState: FileState | undefined = context.workspaceState.get(filePath);
					const specs: ISpecification[] = fileState? fileState.specs : [];

					const inlayHints = specs.flatMap((spec: ISpecification) => {
						let start = new vscode.Position(spec.specRange.start.line, spec.specRange.start.character)
						let end = new vscode.Position(spec.specRange.end.line, spec.specRange.end.character)
						if (range.contains(start) || range.contains(end)) {
							return [
								new vscode.InlayHint(start.translate(0, 2), ` ${spec.preCondition}`),
								new vscode.InlayHint(end, ` ${spec.postCondition}`)
							];
						}
						return [];
					});
					return inlayHints;
				} else {
					return [];
				}
			}
		}
	)

	// Store the first editor in a state.
	context.workspaceState.update("editor", retrieveMainEditor());
	// If none of the tabs has the Guabao label ...
	if(vscode.window.tabGroups.all.flatMap(group => group.tabs).filter(tab => isGuabaoLabel(tab.label)).length === 0) {
		// Initialize the panel.
		panelProvider.createPanel();
		// Show the welcome page.
		panelProvider.showLoading(context.extensionPath);
		// We prevent focusing on the panel instead of the text editor.
		vscode.commands.executeCommand('workbench.action.focusFirstEditorGroup');
	}

	const reloadDisposable = vscode.commands.registerCommand('guabao.reload', async () => {
		// Store the main editor in a state.
		context.workspaceState.update("editor", retrieveMainEditor());
		// Get the path for the current text file.
		const filePath = retrieveMainEditor()?.document.uri.fsPath;
		// Send the request asynchronously.
		const response =  await sendRequest("guabao/reload", {filePath: filePath})
		// ignore the response and get the result from the notification
	});
	context.subscriptions.push(reloadDisposable);

	const refineDisposable = vscode.commands.registerCommand('guabao.refine', async () => {
		// Check if the panel is present before doing anything else.
		const editor = retrieveMainEditor();
		if(panelProvider.initiated() && editor) {
			
			const filePath = editor.document.uri.fsPath;
			const selectionRange = genSelectionRangeWithOffset(editor);
			let specLines = getSpecLinesRange(editor, selectionRange);
			
			if(specLines && filePath) {
				const implText = getImplText(editor, specLines)
				const _ = await sendRequest("guabao/refine", {
					filePath: filePath,
					specLines: specLines.toJson(),
					implText: getImplText(editor, specLines)
				})
			} else {
				vscode.window.showInformationMessage("Cannot refine.");
			}

		} else {
			vscode.window.showInformationMessage("Should not happen.");
		}
		
	});
	context.subscriptions.push(refineDisposable);

	await start();

	const updateNotificationHandlerDisposable = onUpdateNotification(async ({
		filePath,
		specs,
		pos,
		warnings
	}) => {
		const oldFileState: FileState | undefined = context.workspaceState.get(filePath);
		let newFileState: FileState =
			oldFileState
			? {...oldFileState, specs, pos, warnings}
			: {filePath, specs, pos, warnings, errors: []};
		await context.workspaceState.update(filePath, newFileState);
		panelProvider.rerender(newFileState);
		await updateInlayHints(newFileState);

		async function updateInlayHints(newFileState: FileState) {
			let mainEditor = retrieveMainEditor();
			if (mainEditor) {
				let visableRange = mainEditor.visibleRanges.reduce((range1, range2) => range1.union(range2));
				vscode.commands.executeCommand('vscode.executeInlayHintProvider', vscode.Uri.file(newFileState.filePath), visableRange);
			}

		}
	});
	context.subscriptions.push(updateNotificationHandlerDisposable);

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
		panelProvider.rerender(newFileState);
	});
	context.subscriptions.push(errorNotificationHandlerDisposable);
}

export function deactivate() {
	console.log('Deactivating gcl-vscode');
	stop()
}

