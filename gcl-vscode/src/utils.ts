import * as vscode from 'vscode';

// Simply using either vscode.window.activeTextEditor or vscode.window.visibleTextEditors[0] creates bugs.
export function retrieveMainEditor(): vscode.TextEditor {
    // I don't know why, but it works beautifully.
    // P.S. Vince suggested that a text editor contains several tabs, and we should access the path of the tabs instead of that of the editor.
    return vscode.window.visibleTextEditors[0].document.fileName !== 'tasks' ? vscode.window.visibleTextEditors[0] : vscode.window.visibleTextEditors[1];
}
