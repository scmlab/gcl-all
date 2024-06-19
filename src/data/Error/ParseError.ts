import { Position, Range } from "vscode-languageclient";

export type ParseError = LexicalError | SyntacticError;

interface LexicalError {
    tag: "LexicalError";
    position: Position;
}

type SyntacticError = {
    tag: "SyntacticError";
    location: Range;
    message: string;
}[];
