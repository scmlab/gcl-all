import { IHole } from '../../data/FileState'
import { renderRange } from '../Range';
import renderSection from '../Section';

export default function renderHole(hole: IHole) {
  
  const sectionBody: string = /*html */`
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
          <td class="proof-point">Required Type.</td>
          <td>${hole.holeType}</td>
        </tr>
      </table>
    </div>
  `;
  return renderSection(
    "Hole",
    sectionBody,
    `at ${renderRange(hole.holeRange)}`,
    hole.holeID
  );
}
