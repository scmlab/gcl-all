import { Range } from "vscode-languageclient";

type TypeExpression = string;

interface Name {
    symbol: string;
    location?: Range;
}

export type TypeError
    = NotInScope
    | UnifyFailed
    | RecursiveType
    | AssignToConst
    | UndefinedType
    | DuplicatedIdentifiers
    | RedundantNames
    | RedundantExprs
    | MissingArguments
    | KindUnifyFailed
    | PatternArityMismatch;

interface NotInScope {
    tag: "NotInScope";
    symbol: Name;
}

interface UnifyFailed {
    tag: "UnifyFailed";
    location?: Range;
    typeExpressions: [TypeExpression, TypeExpression];
}

interface RecursiveType {
    tag: "RecursiveType";
    typeVariable: Name;
    typeExpression: TypeExpression;
    location?: Range;
}

interface AssignToConst {
    tag: "AssignToConst";
    constSymbol: Name;
}

interface UndefinedType {
    tag: "UndefinedType";
    typeVariable: Name;
}

interface DuplicatedIdentifiers {
    tag: "DuplicatedIdentifiers";
    identifiers: Name[];
}

interface RedundantNames {
    tag: "RedundantNames";
    names: Name[];
}

interface RedundantExprs {
    tag: "RedundantExprs";
    expressions: TypeExpression[];
}

interface MissingArguments {
    tag: "MissingArguments";
    argumentNames: Name[];
}

type KindExpression = string;
interface KindUnifyFailed {
    tag: "KindUnifyFailed";
    location?: Range;
    kindExpressions: [KindExpression, KindExpression];
}

interface PatternArityMismatch {
    tag: "PatternArityMismatch";
    location?: Range;
    expected: number;
    received: number;
}
