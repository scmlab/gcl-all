import { Range } from "vscode-languageclient";
import { renderPosition } from "./Position";
import * as vscode from 'vscode';

export function renderRange(range: Range): string {
    vscode.window.showErrorMessage(JSON.stringify(range))
    // console.log(range)
    return `${renderPosition(range.start)}-${renderPosition(range.end)}`
}