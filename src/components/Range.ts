import { Range } from "vscode-languageclient";
import { renderPosition } from "./Position";

export function renderRange(range: Range): string {
    return `${renderPosition(range.start)}-${renderPosition(range.end)}`
}