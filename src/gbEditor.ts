
import * as vscode from 'vscode';
import * as sysPath from 'path';
import { retrieveMainEditor, guabaoLabel, genSelectionRangeWithOffset } from "./utils"
import { FileState } from './data/FileState';
import renderError from './components/FileState/ErrorSection';
import renderProofObligation from './components/FileState/ProofObligation';
import renderWarning from './components/FileState/WarningSection';

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
	updateFileState(fileState: FileState): void {
		PanelProvider.panel.webview.html = renderFileState(fileState);
	}
	// Show either the welcome page or sections (likely from the LSP server).
	showWelcome(extPath: string): void {
		PanelProvider.panel.webview.html = renderWelcome(extPath);
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
	
	return /* html */`
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
					<h2 class="text-center">Guabao: Program and Prove with GCL.</h2>
				</div>
			</body>
		</html>
	`
}

export function renderFileState(fileState: FileState): string {
	return /* html */`
    	<!DOCTYPE html>
        <html lang="en" ${themeAttr()}>
            <head>
                <title>${guabaoLabel}</title>
                <meta charset="UTF-8">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
            </head>
            <body>  
				${fileState.errors.map(renderError).join()}
				${fileState.pos.map(renderProofObligation).join('')}
				${fileState.warnings.map(renderWarning).join()}
            </body>
        </html>
    `
}