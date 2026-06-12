// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from "vscode";
import { executeOnGclEditor } from "./utils";
import {
  start,
  stop,
  restart,
  sendRequest,
  onFileStateNotification,
} from "./client";
import { GclPanel } from "./gclPanel";
import { IHole, ISpecification, ClientFileState } from "./data/FileState";
import path from "path";

export async function activate(context: vscode.ExtensionContext) {
  console.log("activating gcl-vscode");

  // Persists for the extension lifetime (activate is called once per session).
  // Keyed by filePath; entries are added/updated on gcl/update notifications.
  const fileStateMap = new Map<string, ClientFileState>();

  // Displays pre- and post- conditions as inline hints around specs
  // TODO: Fully display long inlay hints.
  // ^^^^^ P.S. This doesn't seem to be solvable with the current VSCode version. We have to wait.
  const inlayHintsEmitter = new vscode.EventEmitter<void>();
  const inlayHintsDisposable = vscode.languages.registerInlayHintsProvider(
    { scheme: "file", language: "gcl" },
    {
      onDidChangeInlayHints: inlayHintsEmitter.event,
      provideInlayHints(document, visableRange, token): vscode.InlayHint[] {
        let filePath: string = document.uri.fsPath;
        const clientState: ClientFileState | undefined =
          fileStateMap.get(filePath);

        if (clientState === undefined) return [];

        const inlayHints = clientState.specs.flatMap((spec: ISpecification) => {
          const start = new vscode.Position(
            spec.specRange.start.line,
            spec.specRange.start.character,
          );
          const end = new vscode.Position(
            spec.specRange.end.line,
            spec.specRange.end.character,
          );
          if (visableRange.contains(start) || visableRange.contains(end)) {
            const preConditionHint = new vscode.InlayHint(
              start.translate(0, 2),
              `${spec.preCondition}`,
            );
            preConditionHint.paddingLeft = true;
            const postConditionHint = new vscode.InlayHint(
              end,
              `${spec.postCondition}`,
            );
            postConditionHint.paddingLeft = true;
            return [preConditionHint, postConditionHint];
          }
          return [];
        });

        inlayHints.push(
          ...clientState.holes.flatMap((hole: IHole) => {
            const holeRange = hole.holeRange;
            const start = new vscode.Position(
              holeRange.start.line,
              holeRange.start.character,
            );
            const end = new vscode.Position(
              holeRange.end.line,
              holeRange.end.character,
            );
            if (visableRange.contains(start) || visableRange.contains(end)) {
              const preConditionHint = new vscode.InlayHint(
                end.translate(0, -2),
                `${hole.holeID}`,
              );
              preConditionHint.paddingLeft = true;
              return [preConditionHint];
            }

            return [];
          }),
        );

        return inlayHints;
      },
    },
  );
  context.subscriptions.push(inlayHintsDisposable);

  const gclPanel = new GclPanel(context);
  gclPanel.showLoading(context.extensionPath);
  // We prevent focusing on the panel instead of the text editor.
  vscode.commands.executeCommand("workbench.action.focusFirstEditorGroup");

  // When switching tabs to a .gcl file, display the corresponding state in gclPanel
  const changeTabDisposable = vscode.window.tabGroups.onDidChangeTabs(
    (event: vscode.TabChangeEvent) => {
      const changedTab: vscode.Tab = event.changed[0];
      const isFileTab: boolean = "uri" in (changedTab.input as any);
      if (isFileTab) {
        const filePath = (changedTab.input as { uri: vscode.Uri }).uri.fsPath;
        let clientState: ClientFileState | undefined =
          fileStateMap.get(filePath);
        if (clientState) gclPanel.rerender(clientState);
      }
    },
  );
  context.subscriptions.push(changeTabDisposable);

  // Keep the GCL panel's editor group clear of text editors.
  // The panel is a webview that shares an editor group with text editors
  // (created in ViewColumn.Two). If focus is on the panel and the user
  // double-clicks a file in the explorer, VS Code opens that file in the
  // active group, i.e. right next to the panel, instead of in the left
  // editor group. When we detect a text editor landing in the panel's group,
  // move it back to the first group. Moving it changes its viewColumn so this
  // handler won't fire again for it (no infinite loop).
  const keepPanelGroupClearDisposable =
    vscode.window.onDidChangeActiveTextEditor((editor) => {
      if (!editor) return;
      const panelColumn = gclPanel.panel.viewColumn;
      if (panelColumn !== undefined && editor.viewColumn === panelColumn) {
        vscode.commands.executeCommand(
          "workbench.action.moveEditorToFirstGroup",
        );
      }
    });
  context.subscriptions.push(keepPanelGroupClearDisposable);

  const closeDocDisposable = vscode.workspace.onDidCloseTextDocument(
    (document) => {
      fileStateMap.delete(document.uri.fsPath);
    },
  );
  context.subscriptions.push(closeDocDisposable);

  // request gcl/reload
  const reloadDisposable = vscode.commands.registerCommand(
    "gcl.reload",
    () =>
      executeOnGclEditor(async (editor) => {
        const filePath = editor.document.uri.fsPath;
        await sendRequest("gcl/reload", { filePath: filePath });
      }),
  );
  context.subscriptions.push(reloadDisposable);

  // refine gcl/refine
  const refineDisposable = vscode.commands.registerCommand(
    "gcl.refine",
    () =>
      executeOnGclEditor(async (editor) => {
        const filePath = editor.document.uri.fsPath;
        await sendRequest("gcl/refine", {
          filePath: filePath,
          line: editor.selection.start.line, // 0-based
          character: editor.selection.start.character, // 0-based
        });
      }),
  );
  context.subscriptions.push(refineDisposable);

  await start();

  // restart server command
  const restartDisposable = vscode.commands.registerCommand(
    "gcl.restartServer",
    async () => {
      try {
        // No success popup here: the server itself announces
        // "GCL Server Initialized." once it is back up.
        await restart();
      } catch (e: any) {
        vscode.window.showErrorMessage(
          "Failed to restart GCL server: " + e.message,
        );
      }
    },
  );
  context.subscriptions.push(restartDisposable);

  // request gcl/debug
  const debugDisposable = vscode.commands.registerCommand(
    "gcl.debug",
    () =>
      executeOnGclEditor(async (editor) => {
        const filePath = editor.document.uri.fsPath;
        await sendRequest("gcl/debug", { filePath: filePath });
      }),
  );
  context.subscriptions.push(debugDisposable);

  const outputChannel = vscode.window.createOutputChannel("GCL");
  context.subscriptions.push(outputChannel);

  // notification gcl/update
  const updateNotificationHandlerDisposable = onFileStateNotification(
    async ({ filePath, errors, holes, specs, pos, warnings }) => {
      const timestamp = new Date().toLocaleString();
      outputChannel.appendLine(
        `[${timestamp}] Received update for ${filePath}:`,
      );
      outputChannel.appendLine(JSON.stringify({ specs }, null, 2));

      for (const po of pos) {
        po.click = `
          <span class="clickable" data-redex-id="outer">
            outer
            <span class="clickable" data-redex-id="inner">inner text</span>
            text
          </span>
        `;
      }

      let newClientFileState: ClientFileState = {
        errors,
        holes,
        specs,
        pos,
        warnings,
      };

      fileStateMap.set(filePath, newClientFileState);
      gclPanel.rerender(newClientFileState);
      await updateInlayHints(newClientFileState);

      async function updateInlayHints(newClientFileState: ClientFileState) {
        inlayHintsEmitter.fire();
      }
    },
  );
  context.subscriptions.push(updateNotificationHandlerDisposable);

  gclPanel.panel.webview.onDidReceiveMessage(
    (msg) => {
      console.log(msg);
    },
    undefined,
    context.subscriptions,
  );
}

export async function deactivate() {
  console.log("deactivating gcl-vscode");
  try {
    await stop();
  } catch (e: any) {
    console.error("Error stopping client", e);
  }
}
