import { ISpecification } from "../../data/FileState";
import { renderRange } from "../Range";
import renderSection from "../Section";

export default function renderSpecification(specification: ISpecification) {
  const sectionBody: string = /*html */ `
    <div>
      <style scoped>
        .spec-condition {
          color: #959595;
          width: max-content;
          padding-right: 5px;
        }
      </style>
      <table class="proof-obligation">
        <tr>
          <td class="proof-point">Assume.</td>
          <td>${specification.reducedPreCond}</td>
        </tr>
        <tr>
          <td class="proof-point">Require.</td>
          <td>${specification.reducedPostCond}</td>
        </tr>
      </table>
    </div>
  `;
  return renderSection(
    "Specification",
    sectionBody,
    `at ${renderRange(specification.specRange)}`,
    specification.specID,
  );
}
