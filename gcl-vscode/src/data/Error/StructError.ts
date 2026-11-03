import { Range } from "vscode-languageclient";

export interface StructError {
  tag:
    | "MissingInvariant"
    | "MissingPostcondition"
    | "MultiDimArrayAsgnNotImp"
    | "LocalVarExceedScope";
  location?: Range;
}
