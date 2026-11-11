import { Range } from "vscode-languageclient";
import { Error } from "./Error";

type Predicate = string; // Plain text (pretty-printed).
type Html = string; // Rendered HTML fragment.
type Type = string;

export interface ClientFileState {
  errors: Error[];
  holes: IHole[];
  specs: ISpecification[];
  pos: IProofObligation[];
  warnings: IStructWarning[];
  globalProps: Html[];
}

export interface ISpecification {
  specID: string;
  preCondition: Predicate;
  postCondition: Predicate;
  // Current display conditions, including clickable redexes.
  reducedPreCond: Html;
  reducedPostCond: Html;
  specRange: Range;
}

export interface IHole {
  holeID: string;
  holeType: Type;
  holeRange: Range;
}

export interface IProofObligation {
  pred: Html;
  // Current display predicate, including clickable redexes.
  reducedPred: Html;
  strippedPred: Predicate;
  proofExists: Boolean;
  smtResult: string;
  proofLocation?: Range;
  footPrints: IFootPrint[];
  origin?: Range;
}

export type IFootPrint = {
  tag:
    | "IfTotal"
    | "IfBranch"
    | "LoopInvBase"
    | "LoopInvInd"
    | "LoopTermBase"
    | "LoopTermInd";
  range?: Range;
};

export type IStructWarning = MissingBound;

export interface MissingBound {
  tag: "MissingBound";
  range: Range;
}
