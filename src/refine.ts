import * as vscode from 'vscode';
import { RangeWithOffset } from "./utils";
import { start } from 'repl';
import { Position, Range } from 'vscode-languageclient';

export function getSpecText(editor: vscode.TextEditor, specLines: RangeWithOffset): string {
    let implText: string = '';
    for (let i = specLines.startLine - 1; i < specLines.endLine; i++) {
        implText += editor.document.lineAt(i).text;
        if (i + 1 < specLines.endLine) implText += '\n';
    }
    return implText;
}

export function getImplText(editor: vscode.TextEditor, specLines: RangeWithOffset): string {
    let implText: string = '';
    for (let i = specLines.startLine; i < specLines.endLine - 1; i++) {
        implText += editor.document.lineAt(i).text;
        if (i + 1 < specLines.endLine - 1) implText += '\n';
    }
    return implText;
}


export function getImplLinesRange(editor: vscode.TextEditor, specLines: RangeWithOffset): RangeWithOffset {
    const implStartLineNumber: number = specLines.startLine + 1;
    const implStartCol: number = 1;

    let specStartLine: Range = editor.document.lineAt(specLines.startLine - 1).range;
    let specStartLineLength: number = specStartLine.end.character - specStartLine.start.character;
    const implStartOff: number = specLines.startOff + specStartLineLength + 1; // `\n`

    const implEndLineNumber: number = specLines.endLine - 1;
    const implEndCol: number = editor.document.lineAt(implEndLineNumber - 1).range.end.character + 1;

    let specEndLine: Range = editor.document.lineAt(specLines.endLine - 1).range
    let specEndLineLength: number = specEndLine.end.character - specEndLine.start.character
    const implEndOff: number = specLines.endOff - specEndLineLength - 1; // `\n`

    return new RangeWithOffset(
        specLines.path,
        implStartLineNumber,
        implStartCol,
        implStartOff,
        implEndLineNumber,
        implEndCol,
        implEndOff
    )

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

// TODO: Should not scan through the whole source. Start from `selectionRange` instead.
export function getSpecRange(editor: vscode.TextEditor, selectionRange: RangeWithOffset): RangeWithOffset | undefined {
    const text = editor.document.getText();
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