import { Section, Block, Header, HeaderWithButtons, Paragraph, Code, Inline, Icon, Text, Snpt, Link, Sbst, Horz, Vert, Parn } from './section'
import * as vscode from 'vscode';
import * as sysPath from 'path';
import { retrieveMainEditor, guabaoLabel, genSelectionRangeWithOffset } from "./utils"
import { sendRequest } from "./connection";
import { getSubstitutions } from './substitute';

export class Welcome {
	constructor() {}
}

export class PanelProvider {
	static panel: vscode.WebviewPanel;
	initiated(): boolean {
		return PanelProvider.panel !== undefined
	}
	createPanel(): void {
		PanelProvider.panel =
			vscode.window.createWebviewPanel(
				"gbCustom.guabao",
				guabaoLabel,
				{ preserveFocus: true, viewColumn: vscode.ViewColumn.Two },
				{ enableScripts: true }
			);
		
	}
	// Show either the welcome page or sections (likely from the LSP server).
	show(content: Welcome | Section[], extPath: string): void {
		if (content instanceof Welcome) {
			PanelProvider.panel.webview.html = renderWelcome(extPath);
		} else if (content.every(it => it instanceof Section)){
			PanelProvider.panel.webview.html = renderSections(content, extPath);
		}
	}
	// This method handles message sent from within the webview.
	receiveMessage(context: vscode.ExtensionContext) {
		// The decoration type we want to use later.
		// It could not be declared inside the async callback because it would then create different objects every time. 
		const decorationType = vscode.window.createTextEditorDecorationType({
			borderWidth: '1px',
			borderStyle: 'solid',
			overviewRulerColor: 'blue',
			overviewRulerLane: vscode.OverviewRulerLane.Right,
			light: {
				// This color will be used in light color themes.
				borderColor: 'darkblue'
			},
			dark: {
				// This color will be used in dark color themes.
				borderColor: 'lightblue'
			}
		});
		PanelProvider.panel.webview.onDidReceiveMessage(
			async message => {
				switch (message.command) {
					case 'decorate':
						// Currently, we pass the four arguments of the range 1-by-1.
						// With some modification, it might be possible to pass the whole range all at once.
						retrieveMainEditor()?.setDecorations(decorationType, [new vscode.Range(new vscode.Position(message.startLine, message.startChar), new vscode.Position(message.endLine, message.endChar))]);
						return;
					// Reset the decoration when the mouse moves out.
					case 'undecorate':
						retrieveMainEditor()?.setDecorations(decorationType, []);
						return;
					case 'insertProofTemplate': {
						const editor = retrieveMainEditor();
						const range = editor ? genSelectionRangeWithOffset(editor) : undefined;
						await sendRequest("guabao", [
							range?.path, { "tag": "ReqInsertProofTemplate",
								"contents": [
									[
										[range?.path, range?.startLine, range?.startChar, range?.startOff],
										[range?.path, range?.endLine, range?.endChar, range?.endOff]
									],
									message.hash
								]
							}
						]);
						vscode.commands.executeCommand('guabaovlang.reload');
						vscode.commands.executeCommand('workbench.action.focusFirstEditorGroup');
						return;
					}
					case 'substitute': {

						const editor = retrieveMainEditor();
						const path = editor?.document.uri.fsPath;

						const response = await sendRequest("guabao", [
							path, { "tag": "ReqSubstitute",
								"contents": message.redexNumber
							}
						]);
						
						const substitutions = getSubstitutions(response);
						substitutions.map(sub =>
							PanelProvider.panel.webview.postMessage({ command: 'renderSubstitution', redexNumber: sub.redexNumber, inlines: renderInlines(sub.inlines) })
						);
						vscode.commands.executeCommand('workbench.action.focusFirstEditorGroup');
						return;

					}
				}
			},
			undefined,
			context.subscriptions
		);
	}
}

function themeAttr(): string {
	let theme;
	switch(vscode.window.activeColorTheme.kind) {
		case vscode.ColorThemeKind.Light:
		case vscode.ColorThemeKind.HighContrastLight:
			theme = 'data-bs-theme="light"';
			break;
		case vscode.ColorThemeKind.Dark:
		case vscode.ColorThemeKind.HighContrast:
			theme = 'data-bs-theme="dark"';
			break;
	}
	return theme;
}

// The below renderXXXXX functions turn the parsed data structure into HTML.

function renderWelcome(extPath: string): string {
	const webview = PanelProvider.panel.webview;
	const stylePathOnDisk = vscode.Uri.file(sysPath.join(extPath, '/asset/bootstrap.min.css'));
	const styleUri = webview.asWebviewUri(stylePathOnDisk);
	
	const welcomeMsg: string = `
		<!DOCTYPE html>
		<html lang="en" ${themeAttr()}>
			<head>
				<title>${guabaoLabel}</title>
				<meta charset="UTF-8">
				<meta name="viewport" content="width=device-width, initial-scale=1.0">
				<link rel='stylesheet' type='text/css' href='${styleUri}'>
			</head>
			<body>
				<div class="container p-3">
					<h2 class="text-center">You started Guabao!</h2>
				</div>
			</body>
		</html>
	`
	return welcomeMsg;
}

function renderSections(sections: Section[], extPath: string): string {
	const webview = PanelProvider.panel.webview;
	// The CSS file path.
	const stylePathOnDisk = vscode.Uri.file(sysPath.join(extPath, '/asset/bootstrap.min.css'));
	const styleUri = webview.asWebviewUri(stylePathOnDisk);

	const content = sections.map(section => {
		// These are the colors we may receive and render.
		switch(section.deco) {
			case 'Plain':
				return `<div class="border rounded p-3">${renderBlocks(section.blocks)}</div>`;
			case 'Red':
				return `<div class="bg-danger border rounded p-3">${renderBlocks(section.blocks)}</div>`;
			case 'Yellow':
				return `<div class="bg-warning border rounded p-3 text-dark">${renderBlocks(section.blocks)}</div>`;
			case 'Blue':
				return `<div class="bg-primary border rounded p-3">${renderBlocks(section.blocks)}</div>`;
			case 'Green':
				return `<div class="bg-success border rounded p-3">${renderBlocks(section.blocks)}</div>`;
		}
	}).join('<br>')

	// These are the Javascript functions to bridge the webview and the extension.
	const script = `
		const vscode = acquireVsCodeApi();
		function decorateCode(startLine, startChar, endLine, endChar) {
			vscode.postMessage({
				command: 'decorate',
				startLine: startLine,
				startChar: startChar,
				endLine: endLine,
				endChar: endChar
			});
		}
		function undecorateCode() {
			vscode.postMessage({
				command: 'undecorate'
			});
		}
		function insertProofTemplate(hash) {
			vscode.postMessage({
				command: 'insertProofTemplate',
				hash: hash
			});
		}
		function notifySubstitute(number) {
			vscode.postMessage({
				command: 'substitute',
				redexNumber: number
			});
		}
		window.addEventListener('message', event => {
			const message = event.data; // The JSON data our extension sent
			const redexNumber = message.redexNumber;
			switch (message.command) {
				case 'renderSubstitution':
					document.getElementById("redex" + redexNumber).outerHTML = message.inlines;
					break;
			}
		});
	`

	return `
		<!DOCTYPE html>
		<html lang="en" ${themeAttr()}>
			<head>
				<title>${guabaoLabel}</title>
				<meta charset="UTF-8">
				<meta name="viewport" content="width=device-width, initial-scale=1.0">
				<link rel='stylesheet' type='text/css' href='${styleUri}'>
			</head>
			<body>
				<div class="container p-3">
					${content}
				</div>
				<script>
					${script}
				</script>
			</body>
		</html>
	`;
}

function renderBlocks(blocks: Block[]): string {
	return blocks.map(block => {
		if (block instanceof Header) {
			return renderHeader(block);
		}
		if (block instanceof HeaderWithButtons) {
			return renderHeaderWithButtons(block);
		}
		if (block instanceof Paragraph) {
			return `<div>${renderInlines(block.inlines)}</div>`
		}
		if (block instanceof Code) {
			return `<div>${renderInlines(block.inlines)}</div>`
		}
	}).join("<br>")
}

function renderHeader(header: Header): string {
	return `<h2 class="text-center">${header.text} ${renderRange(header.range) ?? ""}</h2>`;
}

function renderHeaderWithButtons(header: HeaderWithButtons): string {
	const buttonName = header.anchorLoc === undefined ? "Insert Proof Template" : renderRange(header.anchorLoc);
	const disabled = header.anchorLoc === undefined ? "" : "disabled";
	return `
		<div class="container text-center">
			<div class="row">
				<div class="col col-sm-6">
					<h2 class="text-center">${header.headerText} ${renderRange(header.headerLoc) ?? ""}</h2>
				</div>
				<div class="col col-sm-6">
					<button type="button" class="btn btn-primary" onclick="insertProofTemplate('${header.anchorText}')" ${disabled}>${buttonName}</button>
				</div>
			</div>
		</div>
	`;
}

function renderInlines(inlines: Inline[]): string {
	return inlines.map(inline => {
		if(inline instanceof Icon) {
			return "ICON"; // Placeholder.
		}
		if(inline instanceof Text) {
			return inline.text;
		}
		if(inline instanceof Snpt) {
			return ` <code>${renderInlines(inline.inlines)}</code> `;
		}
		if(inline instanceof Link) {
			return `
				<span
					data-bs-toggle="tooltip"
					title="${renderRange(inline.range)}"
					onmouseover="decorateCode(${inline.range?.start.line}, ${inline.range?.start.character}, ${inline.range?.end.line}, ${inline.range?.end.character})"		
					onmouseout="undecorateCode()"
				>${renderInlines(inline.inlines)}</span>`; // Omit `inline.classNames` because I don't know what that is.
		}
		if(inline instanceof Sbst) {
			return `
				<span
					onclick="notifySubstitute(${inline.redexNumber})"
					id="redex${inline.redexNumber}"
					style="cursor:pointer"
				>${renderInlines(inline.inlines)}</span>`;
		}
		if(inline instanceof Horz) {
			const columns = inline.columnns.map(col => renderInlines(col)).join("")
			return `${columns}`;
		}
		if(inline instanceof Vert) {
			const rows = inline.rows.map(row => renderInlines(row)).join("<br>")
			return `<br><div class="text-center">${rows}</div><br>`;
		}
		if(inline instanceof Parn) {
			return `( ${renderInlines(inline.inlines)} )`;
		}
	}).join("")
}

function renderRange(range: vscode.Range | undefined): string {
	if(range === undefined) {
		return "";
	} else {
		return `${adjustLineOrChar(range?.start.line)}:${adjustLineOrChar(range?.start.character)}-${adjustLineOrChar(range?.end.line)}:${adjustLineOrChar(range?.end.character)}`
	}
}

// This fixes the off-by-one error.
function adjustLineOrChar(num: number | undefined): number | undefined {
	if(num === undefined) {
		return undefined;
	} else {
		return num + 1;
	}
}