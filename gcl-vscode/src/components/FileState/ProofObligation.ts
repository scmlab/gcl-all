import { IProofObligation } from "../../data/FileState";
import { renderRange } from "../Range";
import renderSection from "../Section";

export default function renderProofObligation(
  proofObligation: IProofObligation,
): string {
  const isTrivial =
    !proofObligation.assumption || proofObligation.assumption.trim() === "True";
  const renderedExpression = isTrivial
    ? `<span>${proofObligation.goal}</span>`
    : `<span>${proofObligation.assumption}</span><span class="implication">=&gt;</span><span>${proofObligation.goal}</span>`;
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
        /* reducible spots (carry data-redex); highlight on hover */
        .gcl-redex {
          border-radius: 3px;
          transition: background-color 0.08s ease-in-out;
        }
        .gcl-redex:hover {
          background-color: rgba(100, 150, 255, 0.25);
          cursor: pointer;
        }
        /* with nested redexes, only highlight the innermost one under the cursor */
        .gcl-redex:has(.gcl-redex:hover) {
          background-color: transparent;
        }
      </style>
      <table class="proof-obligation">
      <tr>
        <td class="proof-obligation-expression">${renderedExpression}</td>
        <td class="proof-point">Click.</td>
        <td>${proofObligation.click}</td>
      </tr>
      </table>
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

// assumption: Predicate;
// goal: Predicate;
// hash: string;
// proofLocation?: Range;
// origin: {
//     tag?: "Abort" | "Skip" | "Spec" | "Assignment" | "Assertion" | "Conditional" | "Loop Invariant" | "Loop Termination";
//     location: {
//         filePath: string;
//     } & Range;
//     explanation?: string;
// }
