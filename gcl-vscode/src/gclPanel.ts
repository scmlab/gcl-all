
import * as vscode from 'vscode';
import { ClientState } from './data/ClientState';
import renderError from './components/FileState/ErrorSection';
import renderProofObligation from './components/FileState/ProofObligation';
import renderWarning from './components/FileState/WarningSection';
import renderSpecification from './components/FileState/Specification';

export class Welcome {
	constructor() {}
}

export class GclPanel {
	static readonly titleLabel = "GCL";
	panel: vscode.WebviewPanel;
	constructor() {
		this.panel = vscode.window.createWebviewPanel(
			"gcl",
			GclPanel.titleLabel,
			{ preserveFocus: true, viewColumn: vscode.ViewColumn.Two },
			{ enableScripts: true }
		);
	}
	show(html: string): void {
		this.panel.webview.html = html
	}
	rerender(clientState: ClientState): void {
		this.panel.webview.html = renderClientState(clientState);
	}
	// Show either the welcome page or sections (likely from the LSP server).
	showLoading(extPath: string): void {
		this.panel.webview.html = renderLoading(extPath);
	}
	
}

function renderLoading(extPath: string): string {
	return /* html */`
		<!DOCTYPE html>
		<html lang="en">
			<head>
				<title>${GclPanel.titleLabel}</title>
				<meta charset="UTF-8">
				<meta name="viewport" content="width=device-width, initial-scale=1.0">
			</head>
			<body>
				<h2 style="
					color: var(--vscode-foreground);
					background-color: var(--vscode-sideBar-background)
				">Loading</h2>
			</body>
		</html>
	`
}

function renderClientState(clientState: ClientState): string {
	return /* html */`
    	<!DOCTYPE html>
        <html lang="en">
            <head>
                <title>${GclPanel.titleLabel}</title>
                <meta charset="UTF-8">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
            </head>
            <body>
				${clientState.errors.map(renderError).join('')}
				${clientState.warnings.map(renderWarning).join('')}
				${clientState.specs.map(renderSpecification).join('')}
				${clientState.pos.map(renderProofObligation).join('')}
            </body>
        </html>
    `
}