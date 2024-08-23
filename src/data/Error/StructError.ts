import { Loc } from "../Loc";

export interface StructError {
    tag: "MissingAssertion" | "MissingPostcondition" | "MultiDimArrayAsgnNotImp" | "LocalVarExceedScope";
    location?: Loc;
}