import * as vscode from 'vscode';
import { RangeWithOffset } from "./utils";
import { start } from 'repl';
import { Position } from 'vscode-languageclient';

export function getImplText(editor: vscode.TextEditor, specLines: RangeWithOffset): string {
    let implText: string = '';
    for (let i = specLines.startLine; i < specLines.endLine - 1; i++) {
        implText += editor.document.lineAt(i).text;
        if (i + 1 < specLines.endLine - 1) implText += '\n';
    }
    return implText;
}

export function getSpecLinesRange(editor: vscode.TextEditor, selectionRange: RangeWithOffset): RangeWithOffset | undefined {
    const specRange = getSpecRange(editor, selectionRange);
    if (!specRange) return undefined;
    const specLinesStart: Position = editor.document.lineAt(specRange.startLine + 1).range.start
    const specLinesStartOffset: number = specRange.startOff - (specRange.startChar - specLinesStart.character);
    const specLinesRange =  new RangeWithOffset(
        specRange.path,
        specRange.startLine,
        1,
        specLinesStartOffset,
        specRange.endLine,
        specRange.endChar,
        specRange.endOff
    )
    return specLinesRange 
}

export function getSpecRange(editor: vscode.TextEditor, selectionRange: RangeWithOffset): RangeWithOffset | undefined {
    const text = editor?.document.getText() ?? "";
    let specs: RangeWithOffset[] = []; 
    let lineCount = 1;
    let colCount = 1;
    let foundLeftBracket: boolean = false;
    let foundRightExclamation: boolean = false;
    let nestMap = new Map<number, [number, number, number]>();
    let nestLevel = 0;
    let extraDelimiter = false; 
    [...text].forEach((ch, index) => {
        if(ch !== "]") {
            foundRightExclamation = false;
        }
        if(ch === "\n") {
            lineCount += 1;
            colCount = 1;
            return;
        }
        if(ch === "[") {
            foundLeftBracket = true;
        }
        else {
            if(foundRightExclamation && ch === "]") {
                if(nestLevel <= 0) {
                    extraDelimiter = true;   
                } else {
                    specs.push(
                        new RangeWithOffset(
                            selectionRange?.path ?? "",
                            nestMap.get(nestLevel)![0],
                            nestMap.get(nestLevel)![1],
                            nestMap.get(nestLevel)![2],
                            lineCount,
                            colCount + 1,
                            index + 1
                        )
                    );
                    nestMap.delete(nestLevel);
                    nestLevel -= 1;
                }
            }
            else if(foundLeftBracket && ch === "!") {
                nestLevel += 1;
                nestMap.set(nestLevel, [lineCount, colCount - 1, index - 1])
            } else if(ch === "!") {
                foundRightExclamation = true;
            }
            foundLeftBracket = false;
        }
        colCount += 1;
    });
    if(extraDelimiter || nestLevel > 0) {
        return undefined;
    }
    for(let spec of specs) {
        if(selectionRange?.startOff! <= spec.startOff
            || selectionRange?.endOff! >= spec.endOff
            || spec.startLine < 0
            || spec.startChar < 0
            || spec.startOff < 0
            || spec.endLine < 0
            || spec.endChar < 0
            || spec.endOff < 0) {
            continue;
        } else {
            return spec;
        }
    }
    return undefined;
}

export function specContent(range: RangeWithOffset | undefined): RangeWithOffset | undefined {
    if(range === undefined) {
        return undefined;
    } else {
        return new RangeWithOffset(range.path, range.startLine, range.startChar + 2, range.startOff + 2, range.endLine, range.endChar - 2, range.endOff - 2);
    }
}