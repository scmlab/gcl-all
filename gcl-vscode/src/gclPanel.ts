import * as vscode from "vscode";
import { ClientFileState } from "./data/FileState";
import renderError from "./components/FileState/ErrorSection";
import renderProofObligation from "./components/FileState/ProofObligation";
import renderWarning from "./components/FileState/WarningSection";
import renderSpecification from "./components/FileState/Specification";
import renderHole from "./components/FileState/Hole";

export class Welcome {
  constructor() {}
}

export class GclPanel {
  static readonly titleLabel = "GCL";
  panel: vscode.WebviewPanel;
  cssUri: vscode.Uri;
  jsUri: vscode.Uri;

  constructor(context: vscode.ExtensionContext) {
    this.panel = vscode.window.createWebviewPanel(
      "gcl",
      GclPanel.titleLabel,
      { preserveFocus: true, viewColumn: vscode.ViewColumn.Two },
      { enableScripts: true },
    );

    this.cssUri = this.panel.webview.asWebviewUri(
      vscode.Uri.joinPath(context.extensionUri, "media", "styles.css"),
    );
    this.jsUri = this.panel.webview.asWebviewUri(
      vscode.Uri.joinPath(context.extensionUri, "media", "main.js"),
    );
  }
  show(html: string): void {
    this.panel.webview.html = html;
  }
  rerender(clientState: ClientFileState): void {
    this.panel.webview.html = renderClientFileState(
      clientState,
      this.panel.webview.cspSource,
      this.cssUri,
      this.jsUri,
    );
  }
  // Show either the welcome page or sections (likely from the LSP server).
  showLoading(extPath: string): void {
    this.panel.webview.html = renderLoading(extPath);
  }
}

function renderLoading(extPath: string): string {
  return /* html */ `
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <title>${GclPanel.titleLabel}</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta
          http-equiv="Content-Security-Policy"
          content="default-src 'none'; style-src 'unsafe-inline'"
        />
      </head>
      <body>
        <h2 style="
          color: var(--vscode-foreground);
          background-color: var(--vscode-sideBar-background)
        ">Loading</h2>
      </body>
    </html>
  `;
}

function renderClientFileState(
  clientState: ClientFileState,
  cspSource: string,
  cssUri: vscode.Uri,
  jsUri: vscode.Uri,
): string {
  return /* html */ `
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <title>${GclPanel.titleLabel}</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta
          http-equiv="Content-Security-Policy"
          content="default-src 'none'; img-src ${cspSource} https:; script-src ${cspSource}; style-src 'unsafe-inline' ${cspSource};"
        />

        <link href="${cssUri}" rel="stylesheet">
      </head>
      <body>
        ${clientState.errors.map(renderError).join("")}
        ${clientState.warnings.map(renderWarning).join("")}
        ${clientState.holes.map(renderHole).join("")}
        ${clientState.specs.map(renderSpecification).join("")}
        ${clientState.pos.map(renderProofObligation).join("")}

        <script src="${jsUri}"></script>
      </body>
    </html>
  `;
}
