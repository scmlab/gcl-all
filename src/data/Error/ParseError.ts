import { Position, Range } from "vscode-languageclient";
import { Loc } from "../Loc";

export type ParseError = LexicalError | SyntacticError;

interface LexicalError {
    tag: "LexicalError";
    position: Position;
}

interface SyntacticError {
    tag: "SyntacticError";
    locatedSymbols: {
        location?: Loc;
        symbol: string;
    }[];
    message: string;
}[];
