import * as vscode from 'vscode';

// Simply using either vscode.window.activeTextEditor or vscode.window.visibleTextEditors[0] creates bugs.
export function retrieveMainEditor(): vscode.TextEditor | undefined {
    // I don't know why, but it works beautifully.
    // P.S. Vince suggested that a text editor contains several tabs, and we should access the path of the tabs instead of that of the editor.
    return vscode.window.visibleTextEditors[0].document.fileName !== 'tasks' ? vscode.window.visibleTextEditors[0] : vscode.window.visibleTextEditors[1];
}

export const guabaoLabel = "GCL";

export function isGuabaoLabel(label: string): boolean {
    return label === guabaoLabel
}

export class RangeWithOffset {
    constructor(
        public path: string,
        public startLine: number,
        public startChar: number,
        public startOff: number,
        public endLine: number,
        public endChar: number,
        public endOff: number
    ) {}
    toJson() {
        return [
            [this.path, this.startLine, this.startChar, this.startOff],
            [this.path, this.endLine, this.endChar, this.endOff]
        ];
    }
    toVscodeRange() {
        return new vscode.Range(
            new vscode.Position(this.startLine - 1, this.startChar - 1),
            new vscode.Position(this.endLine - 1, this.endChar - 1)
        );
    }
}

// This function returns a RangeWithOffset object to be used in various places.
export function genSelectionRangeWithOffset(editor: vscode.TextEditor): RangeWithOffset {
	const path = editor?.document.uri.fsPath;
	const selection = editor?.selection;
	// Note that the position is prone to off-by-one error.
	const startLine = (selection?.start.line ?? 0) + 1;
	const startChar = (selection?.start.character ?? 0) + 1;
	// Not sure if the default value Position(0, 0) is a good idea,
	// but at least I haven't encountered any problems. 
	const startOff = editor?.document.offsetAt(selection?.start || new vscode.Position(0, 0));
	const endLine = (selection?.end.line ?? 0) + 1;
	const endChar = (selection?.end.character ?? 0) + 1;
	const endOff = editor?.document.offsetAt(selection?.end || new vscode.Position(0, 0));
	return new RangeWithOffset(path, startLine, startChar, startOff, endLine, endChar, endOff);
}