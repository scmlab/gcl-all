import * as vscode from "vscode";

/*
// Simply using either vscode.window.activeTextEditor or vscode.window.visibleTextEditors[0] creates bugs.
export function retrieveMainEditor(): vscode.TextEditor {
  // I don't know why, but it works beautifully.
  // P.S. Vince suggested that a text editor contains several tabs, and we should access the path of the tabs instead of that of the editor.
  return vscode.window.visibleTextEditors[0].document.fileName !== "tasks"
    ? vscode.window.visibleTextEditors[0]
    : vscode.window.visibleTextEditors[1];
}
*/

/**
 * Retrieves the relevant GCL editor.
 * 1. If the active editor is a GCL file, return it.
 * 2. If the focus is elsewhere (e.g., on a webview panel), find the first visible GCL editor.
 * 3. Excludes files named "tasks" and non-GCL files.
 */
export function retrieveGclEditor(): vscode.TextEditor | undefined {
  const activeEditor = vscode.window.activeTextEditor;
  if (
    activeEditor?.document.languageId === "gcl" &&
    activeEditor.document.uri.scheme === "file" &&
    activeEditor.document.fileName !== "tasks"
  ) {
    return activeEditor;
  }

  return vscode.window.visibleTextEditors.find(
    (editor) =>
      editor.document.languageId === "gcl" &&
      editor.document.uri.scheme === "file" &&
      editor.document.fileName !== "tasks",
  );
}

/**
 * Executes a callback if a valid GCL editor is found.
 */
export async function executeOnGclEditor(
  callback: (editor: vscode.TextEditor) => Promise<void>,
) {
  const editor = retrieveGclEditor();
  if (editor) {
    await callback(editor);
  }
}
