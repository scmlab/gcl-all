import { IProofObligation } from "../../data/FileState";
import { renderRange } from "../Range";
import renderSection from "../Section";

export default function renderProofObligation(
  proofObligation: IProofObligation,
): string {
  const renderedExpression = `<span>${proofObligation.reducedPred}</span>`;
  const isQED = proofObligation.smtResult === "Q.E.D.";
  const isFalsifiable = proofObligation.smtResult.startsWith("Falsifiable");
  const isTimeout = proofObligation.smtResult === "Timeout";
  const proofResult = isQED
    ? "Q.E.D."
    : isFalsifiable
      ? "Failed"
      : isTimeout
        ? "Timeout"
        : "Unknown";
  const proofDetails = isTimeout
    ? "The SMT prover exceeded the time limit for this proof."
    : proofObligation.smtResult === "Unknown"
      ? "The SMT prover cannot determine this proof because an exponent is symbolic."
      : proofObligation.smtResult;
  const sectionBody: string = /*html */ `
    <div>
      <style scoped>
        .proof-obligation-expression {
          white-space: pre-wrap;
          overflow-wrap: anywhere;
          text-align: left;
        }
        .implication {
          color: #959595;
          padding: 0 6px;
        }
        .proof-obligation-smt-result {
          position: relative;
          display: inline-block;
          margin: 0;
          cursor: help;
        }
        .proof-obligation-smt-result .proof-obligation-smt-result-hover {
          visibility: hidden;
          opacity: 0;
          pointer-events: none;
          position: absolute;
          z-index: 2;
          bottom: 100%;
          right: 0;
          box-sizing: border-box;
          width: max-content;
          min-width: min(160px, calc(100vw - 56px));
          max-width: min(340px, calc(100vw - 56px));
          max-height: min(320px, 50vh);
          overflow: auto;
          padding: 8px 12px;
          background: var(--vscode-editorHoverWidget-background);
          color: var(--vscode-editorHoverWidget-foreground);
          border: 1px solid var(--vscode-editorHoverWidget-border);
          border-radius: 4px;
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.25);
          font-family: var(--vscode-editor-font-family, monospace);
          font-size: 12px;
          font-weight: normal;
          text-align: left;
          line-height: 1.45;
          white-space: pre-wrap;
          overflow-wrap: anywhere;
          transition: opacity 120ms ease;
        }
        .proof-obligation-smt-result:hover .proof-obligation-smt-result-hover,
        .proof-obligation-smt-result:focus-within .proof-obligation-smt-result-hover {
          visibility: visible;
          opacity: 1;
          pointer-events: auto;
        }
      </style>
      <table class="proof-obligation">
      <tr>
        <td class="proof-obligation-expression">${renderedExpression}</td>
      </tr>
      </table>
    </div>
  `;
  const footer = `
    <p class="proof-obligation-smt-result" tabindex="0">
      <strong>SMT Proof: ${proofResult}<span class="proof-obligation-smt-result-hover">${proofDetails}</span></strong>
    </p>
  `;

  return renderSection(
    "Proof Obligation",
    sectionBody,
    proofObligation.origin.location &&
      `at ${renderRange(proofObligation.origin.location)}`,
    `<button class="proof" data-pred="${proofObligation.strippedPred}" ${proofObligation.proofExists ? "disabled" : ""}>Proof</button>`,
    footer,
  );
}

// pred: Predicate;
// hash: string;
// proofLocation?: Range;
// origin: {
//     tag?: "Abort" | "Skip" | "Spec" | "Assignment" | "Assertion" | "Conditional" | "Loop Invariant" | "Loop Termination";
//     location: {
//         filePath: string;
//     } & Range;
//     explanation?: string;
// }
