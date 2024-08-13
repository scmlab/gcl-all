import { Position } from "vscode-languageclient";

export function renderPosition(position: Position): string {
    return `${position.line + 1}:${position.character + 1}`
}