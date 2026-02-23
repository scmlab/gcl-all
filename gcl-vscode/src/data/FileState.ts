import { Range } from "vscode-languageclient";

type Predicate = string;
type Type = string;

export interface FileState {
    holes: IHole[];
	specs: ISpecification[];
	pos: IProofObligation[];
	warnings: IStructWarning[];
}

export interface ISpecification {
	specID: string;
	preCondition: Predicate;
	postCondition: Predicate;
	specRange: Range;
}

export interface IHole {
    holeID: string;
    holeType: Type;
    holeRange: Range;
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
