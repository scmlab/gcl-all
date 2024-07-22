// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { retrieveMainEditor, genSelectionRangeWithOffset, isGuabaoLabel } from './utils'
import { start, stop, sendRequest, onUpdateFileStateNotification } from "./connection";
import { getSpecRange, specContent } from "./refine";
import { Welcome, PanelProvider } from './gbEditor';
import { getSpecs } from './spec'
import { getSections } from './section'
import { FileState } from './data/FileState';
import { ProtocolNotificationType } from 'vscode-languageclient';
import { LanguageClient } from 'vscode-languageclient/node';


export async function activate(context: vscode.ExtensionContext) {
	console.log('GuaBao VLang Mode is now active!');
	const panelProvider = new PanelProvider();

	// Provide inlay hints for the text editor.
	// TODO: Fully display long inlay hints.
	// ^^^^^ P.S. This doesn't seem to be solvable with the current VSCode version. We have to wait.
	// TODO: Do not display inlay artifacts.
	vscode.languages.registerInlayHintsProvider(
		{ scheme: 'file', language: 'guabao' },
		{
			provideInlayHints(document, range, token): vscode.InlayHint[] {
				// We check the editor in the state is what we really want. Else, do nothing.
				if (context.workspaceState.get("editor") === retrieveMainEditor()) {
					const specs = getSpecs(context.workspaceState.get("response"));
					const inlayHints = specs.flatMap(s => 
						[new vscode.InlayHint(s.range.start, ` ${s.pre}`), new vscode.InlayHint(s.range.end, `${s.post} `)]
					);
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
		panelProvider.showWelcome(context.extensionPath);
		// We prevent focusing on the panel instead of the text editor.
		vscode.commands.executeCommand('workbench.action.focusFirstEditorGroup');
	}

	const reloadDisposable = vscode.commands.registerCommand('guabao.reload', async () => {
		// Store the main editor in a state.
		context.workspaceState.update("editor", retrieveMainEditor());
		// Get the path for the current text file.
		const filePath = retrieveMainEditor()?.document.uri.fsPath;
		// Send the request asynchronously.
		const _ = sendRequest("guabao/reload", {filePath: filePath})
		// ignore the response and get the result from the notification
	});
	context.subscriptions.push(reloadDisposable);

	const refineDisposable = vscode.commands.registerCommand('guabao.refine', async () => {
		// Check if the panel is present before doing anything else.
		if(panelProvider.initiated()) {
			
			const editor = retrieveMainEditor();
			const filePath = editor?.document.uri.fsPath;
			const selectionRange = editor ? genSelectionRangeWithOffset(editor) : undefined;
			let specRange = getSpecRange(editor, selectionRange);
			
			if(specRange && filePath) {
				const _ = sendRequest("guabao/reload", {
					filePath: filePath,
					specRange: specRange.toJson(),
					specText: editor?.document.getText(specContent(specRange)?.toVscodeRange()).trim()
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
	const updateNotificationHandlerDisposable = onUpdateFileStateNotification(async (fileState: FileState) => {
		vscode.window.showInformationMessage('Update notification received.');
		await context.workspaceState.update(fileState.filePath, fileState);
		panelProvider.updateFileState(fileState);
		
	});
	context.subscriptions.push(updateNotificationHandlerDisposable);
}

export function deactivate() {
	console.log('Deactivating gcl-vscode');
	stop()
	console.log('Bye!');
}

