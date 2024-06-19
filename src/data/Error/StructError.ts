import { Location } from "vscode-languageclient";

export interface StructError {
    tag: "MissingAssertion" | "MissingPostcondition" | "MultiDimArrayAsgnNotImp" | "LocalVarExceedScope";
    location: Location;
}