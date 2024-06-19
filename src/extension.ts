// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { retrieveMainEditor, genSelectionRangeWithOffset, isGuabaoLabel } from './utils'
import { start, stop, sendRequest } from "./connection";
import { getSpecRange, specContent } from "./refine";
import { Welcome, PanelProvider } from './gbEditor';
import { getSpecs } from './spec'
import { getSections } from './section'

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

	// This is the code for 'Guabao start'.
	// The further below disposables register other commands.
	const startDisposable = vscode.commands.registerCommand('guabao.start', () => {
		// Store the first editor in a state.
		context.workspaceState.update("editor", retrieveMainEditor());
		// If none of the tabs has the Guabao label ...
		if(vscode.window.tabGroups.all.flatMap(group => group.tabs).filter(tab => isGuabaoLabel(tab.label)).length === 0) {
			// Initialize the panel.
			panelProvider.createPanel();
			// Handle message sent from webview such as decoration.
			panelProvider.receiveMessage(context);
			// Show the welcome page.
			panelProvider.show(new Welcome(), context.extensionPath);
			// We prevent focusing on the panel instead of the text editor.
			vscode.commands.executeCommand('workbench.action.focusFirstEditorGroup');
		}
	});
	context.subscriptions.push(startDisposable);

	const reloadDisposable = vscode.commands.registerCommand('guabao.reload', async () => {
		// Store the main editor in a state.
		context.workspaceState.update("editor", retrieveMainEditor());
		// Get the path for the current text file.
		const path = retrieveMainEditor()?.document.uri.fsPath;
		// Send the request asynchronously.
		const response = await sendRequest("guabao/reload", [path, { "tag": "ReqReload" }])
		// We use another shared state to cache the response.
		context.workspaceState.update("response", response);
		if(panelProvider.initiated()) {
			// Parse the response using functions in section.ts.
			const parsedResponse = getSections(response);
			// Tell the panel provider to turn the parsed response into HTML.
			panelProvider.show(parsedResponse, context.extensionPath);
			// Again, we prevent focusing on the panel instead of the text editor.
			vscode.commands.executeCommand('workbench.action.focusFirstEditorGroup');
		} else {
			// If the panel is not present, tell the user to run 'Guabao start' first.
			vscode.window.showInformationMessage("Please run 'Guabao start' first!");
		}
	});
	context.subscriptions.push(reloadDisposable);

	const refineDisposable = vscode.commands.registerCommand('guabao.refine', async () => {
		// Check if the panel is present before doing anything else.
		if(panelProvider.initiated()) {
			
			const editor = retrieveMainEditor();
			const selectionRange = editor ? genSelectionRangeWithOffset(editor) : undefined;
			let specRange = getSpecRange(editor, selectionRange);
			
			if(specRange !== undefined) {
				const response = await sendRequest("guabao", [
					selectionRange?.path, { "tag": "ReqRefine2",
						"contents": [
							specRange?.toJson(),
							editor?.document.getText(specContent(specRange)?.toVscodeRange()).trim()
						]
					}
				]);
				const parsedResponse = getSections(response);
				panelProvider.show(parsedResponse, context.extensionPath);
			} else {
				vscode.window.showInformationMessage("Cannot refine.");
			}

		} else {
			vscode.window.showInformationMessage("Please run 'Guabao start' first!");
		}
		
	});
	context.subscriptions.push(refineDisposable);

	// This is only for testing purpose.
	const helloWorldDisposable = vscode.commands.registerCommand('guabao.helloworld', async () => {

		const editor = retrieveMainEditor();
		const range = editor ? genSelectionRangeWithOffset(editor) : undefined;

		await sendRequest("guabao", [
			range?.path, { "tag": "ReqHelloWorld",
				"contents": [
					[range?.path, range?.startLine, range?.startChar, range?.startOff],
					[range?.path, range?.endLine, range?.endChar, range?.endOff]
				]
			}
		]);

	});
	context.subscriptions.push(helloWorldDisposable);

	// In the beginning, we planned to create a server module path and pass to connection.start() as arg.
	// However, now I run the gcl backend directly instead of using it as a Node.js module.
	const server_module = context.asAbsolutePath("");
	start(server_module);
}

export function deactivate() {
	console.log('Deactivating GuaBao VLang Mode');
	stop()
	console.log('Bye!');
}

