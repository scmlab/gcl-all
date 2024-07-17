import { Location } from "vscode";
import { Range } from "vscode-languageclient";
import { Error } from "./Error"

type Predicate = string;

export interface FileState {
    filePath: string;
	specs: ISpecification[];
	pos: IProofObligation[];
	warnings: IStructWarning[];
    errors: Error[];
}

export interface ISpecification {
	id: number;
	preCondition: Predicate;
	postCondition: Predicate;
	specRange: Range;
}
export interface IProofObligation {
    assumption: Predicate;
    goal: Predicate;
    hash: string;
    proofLocation?: Range;
    origin: {
        tag: "Abort" | "Skip" | "Spec" | "Assignment" | "Assertion" | "Conditional" | "Loop invariant" | "Loop termination";
        location: {
            filePath: string;
        } & Range;
        explanation?: string;
    }
}

export interface IStructWarning {
    tag: "MissingBound" | "ExcessBound";
    range: Range;
}
