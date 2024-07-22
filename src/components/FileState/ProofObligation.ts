
import { IProofObligation } from '../../data/FileState'
import { renderRange } from '../Range'
import renderSection from '../Section'

export default function renderProofObligation(proofObligation: IProofObligation): string {
  const sectionBody: string = /*html */`
    <div>
      <style scoped>
        .proof-point {
          color: #959595;
          width: max-content;
          padding-right: 5px;
        }
        td {
          vertical-align: top;
        }
      </style>
      <table class="proof-obligation">
      <tr>
        <td class="proof-point">Assume.</td>
        <td>${proofObligation.assumption}</td>
      </tr>
      <tr>
        <td class="proof-point">Prove.</td>
        <td>${proofObligation.goal}</td>
      </tr>
      </table>
    </div>
  `;
  return renderSection(
    proofObligation.origin.tag,
    sectionBody,
    proofObligation.origin.location && renderRange(proofObligation.origin.location),
    proofObligation.hash.slice(0, 6).toUpperCase()
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