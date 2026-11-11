import { IFootPrint, IProofObligation } from "../../data/FileState";
import { renderRange } from "../Range";
import renderSection from "../Section";

function renderFootPrint(footPrint: IFootPrint): string {
  return `${footPrint.tag}${footPrint.range ? ` at ${renderRange(footPrint.range)}` : ""}`;
}

export default function renderProofObligation(
  proofObligation: IProofObligation,
): string {
  const renderedExpression = `<span>${proofObligation.reducedPred}</span>`;
  const isQED = proofObligation.smtResult === "Q.E.D.";
  const isFalsifiable = proofObligation.smtResult.startsWith("Falsifiable");
  const isTimeout = proofObligation.smtResult === "Timeout";
  const isUnknown = proofObligation.smtResult.startsWith("Unknown");
  const proofResult = isQED
    ? "Verified"
    : isFalsifiable
      ? "Falsified"
      : isTimeout
        ? "⏱"
        : "?";
  const proofDetails = isTimeout
    ? "Timeout.\n\nThe SMT prover exceeded the time limit for this proof."
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

      </style>
      <table class="proof-obligation">
      <tr>
        <td class="proof-obligation-expression">${renderedExpression}</td>
      </tr>
      </table>
    </div>
  `;
  const smtResultHover = !isQED
    ? `<span class="proof-obligation-hover-box proof-obligation-hover-box--smt">${proofDetails}</span>`
    : "";
  const codeButton = isTimeout || isUnknown
    ? `<button class="proof" data-pred="${proofObligation.strippedPred}" ${proofObligation.proofExists ? "disabled" : ""}>Proof</button>`
    : "";
  const code = `
    <div>
      <style scoped>
        .proof-obligation-smt-controls {
          display: inline-flex;
          align-items: center;
          gap: 8px;
        }
        .proof-obligation-smt-controls .proof {
          width: auto;
          flex: none;
        }
        .proof-obligation-smt-result {
          margin: 0;
        }
      </style>
      <div class="proof-obligation-smt-controls">
        <p class="proof-obligation-smt-result${isQED ? "" : " proof-obligation-hover-trigger"}" ${isQED ? "" : 'tabindex="0"'}>
          <span ${isFalsifiable ? `style="color: lightcoral"` : ""}>${proofResult}${smtResultHover}</span>
        </p>
        ${codeButton}
      </div>
    </div>
  `;

  const [firstFootPrint, ...restFootPrints] = proofObligation.footPrints;
  const footPrintHover = restFootPrints.length > 0
    ? `
      <span class="proof-obligation-hover-box proof-obligation-hover-box--footprints">
        ${
          restFootPrints
            .map(
              (footPrint) =>
                `
                <span class="footprint-hover-item">
                  <strong>
                    ${footPrint.tag}
                  </strong>
                  ${footPrint.range ? ` at ${renderRange(footPrint.range)}` : ""}
                </span>
                `
            )
            .join("")
        }
      </span>
      `
    : "";
  const subtitle = firstFootPrint
    ? `
      <span ${restFootPrints.length > 0 ? 'class="proof-obligation-hover-trigger" tabindex="0"' : ""}>
        ${firstFootPrint.range ? ` at ${renderRange(firstFootPrint.range)}` : ""}
        ${footPrintHover}
      </span>
      `
    : proofObligation.origin
      ? `${renderRange(proofObligation.origin)}`
      : "";

  return renderSection(
    firstFootPrint ? firstFootPrint.tag : "",
    sectionBody,
    subtitle,
    code,
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
