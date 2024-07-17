import { Position } from "vscode-languageclient";

export function renderPosition(position: Position): string {
    return `${position.line}:${position.character}`
}