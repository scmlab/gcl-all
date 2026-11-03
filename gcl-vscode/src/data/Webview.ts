export type WebviewMessage = ReduceMessage | ProofMessage;

type ReduceMessage = {
  action: "reduce";
  po: [Number];
  redex: Number;
};

type ProofMessage = {
  action: "proof";
  pred: string;
};
