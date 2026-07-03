import { IProofObligation } from "../../data/FileState";
import { renderRange } from "../Range";
import renderSection from "../Section";

export default function renderProofObligation(
  proofObligation: IProofObligation,
): string {
  const renderedExpression = `<span>${proofObligation.reducedPred}</span>`;
  const isQED = proofObligation.smtResult === "Q.E.D.";
  const proofResult = isQED ? "Q.E.D." : "Failed";
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
        .proof-obligation-smt-result { position: relative; display: inline-block; }
        .proof-obligation-smt-result .proof-obligation-smt-result-hover {
          visibility: hidden;
          position: absolute;
          background: var(--vscode-editorHoverWidget-background);
          color: var(--vscode-editorHoverWidget-foreground);
          border: 1px solid var(--vscode-editorHoverWidget-border);
          padding: 4px 8px;
          border-radius: 3px;
          z-index: 1;
          bottom: 125%;
          left: 50%;
          transform: translateX(-50%);

          white-space: pre-line;
          max-width: 340px;
          min-width: 160px;

          text-align: left;
          line-height: 1.5;
          padding: 8px 12px;
          font-size: 13px;

          word-wrap: break-word;
          overflow-wrap: break-word;
        }
        .proof-obligation-smt-result:hover .proof-obligation-smt-result-hover { visibility: visible; }
      </style>
      <table class="proof-obligation">
      <tr>
        <td class="proof-obligation-expression">${renderedExpression}</td>
      </tr>
      </table>
      <div style="text-align: right;">
        <p class="proof-obligation-smt-result">
          <strong>SMT Proof: ${proofResult}<span class="proof-obligation-smt-result-hover">${proofObligation.smtResult}</span></strong>
        </p>
      </div>
    </div>
  `;
  return renderSection(
    "Proof Obligation",
    sectionBody,
    proofObligation.origin.location &&
      `at ${renderRange(proofObligation.origin.location)}`,
    proofObligation.hash.slice(0, 6).toUpperCase(),
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
