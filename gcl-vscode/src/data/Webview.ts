export type WebviewMessage = ReduceMessage | ProofMessage;

export type ReduceTarget =
  | { tag: "ProofObligation"; contents: number }
  | { tag: "SpecificationPrecondition"; contents: number }
  | { tag: "SpecificationPostcondition"; contents: number };

type ReduceMessage = {
  action: "reduce";
  redex: number[];
} & ({ po: number } | { target: ReduceTarget });

type ProofMessage = {
  action: "proof";
  pred: string;
};
