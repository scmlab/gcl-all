import { Range } from "vscode-languageclient"
import { Loc } from "../Loc";

type TypeExpression = string;



interface Name {
    symbol: string;
    location?: Loc;
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
    | MissingArguments;

interface NotInScope {
    tag: "NotInScope";
    symbol: Name;
}

interface UnifyFailed {
    tag: "UnifyFailed";
    location?: Loc;
    typeExpressions: [TypeExpression, TypeExpression];
}

interface RecursiveType {
    tag: "RecursiveType";
    typeVariable: Name;
    typeExpression: TypeExpression;
    location?: Loc;
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
