import { Range } from "vscode-languageclient";
import { Error } from "./Error"

type Predicate = string;

export interface FileState {
	specs: ISpecification[];
	pos: IProofObligation[];
	warnings: IStructWarning[];
}

export interface Errors {
    errors: Error[];
}

export type FileStateNotification = { filePath: string } & FileState;

export type ErrorNotification = { filePath: string } & Errors;

export type ClientState = FileState & Errors;

export interface ISpecification {
	specID: number;
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
        tag: "Abort" | "Skip" | "Spec" | "Assignment" | "Assertion" | "Conditional" | "Loop Invariant" | "Loop Termination";
        location?: Range;
        explanation?: string;
    }
}

export type IStructWarning
    = MissingBound

export interface MissingBound {
    tag: "MissingBound";
    range: Range;
}
