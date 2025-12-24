import { Range } from "vscode-languageclient";

export interface StructError {
    tag: "MissingAssertion" | "MissingPostcondition" | "MultiDimArrayAsgnNotImp" | "LocalVarExceedScope";
    location?: Range;
}