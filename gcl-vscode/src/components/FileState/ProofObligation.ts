import { IProofObligation } from "../../data/FileState";
import { renderRange } from "../Range";
import renderSection from "../Section";

export default function renderProofObligation(
  proofObligation: IProofObligation,
): string {
  const renderedExpression = `<span>${proofObligation.reducedPred}</span>`;
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
      </style>
      <table class="proof-obligation">
      <tr>
        <td class="proof-obligation-expression">${renderedExpression}</td>
      </tr>
      </table>
    </div>
  `;

  return renderSection(
    "Proof Obligation",
    sectionBody,
    proofObligation.origin.location &&
      `at ${renderRange(proofObligation.origin.location)}`,
    `<button class="proof" data-pred="${proofObligation.strippedPred}">Proof</button>`,
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
