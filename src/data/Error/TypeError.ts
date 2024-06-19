import { Location } from "vscode-languageclient"

type TypeExpression = string;

interface Name {
    symbol: string;
    location: Location;
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
    location: Location;
    typeExpressions: [TypeExpression, TypeExpression];
}

interface RecursiveType {
    tag: "RecursiveType";
    typeVariable: Name;
    typeExpression: TypeExpression;
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
