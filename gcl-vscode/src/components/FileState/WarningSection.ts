
import { IStructWarning } from '../../data/FileState'
import { renderRange } from '../Range'
import renderSection from '../Section'

export default function renderWarning(warning: IStructWarning) {
    let sectionBody: string = ""
    switch (warning.tag) {
        case "MissingBound":
            sectionBody = "A bound is required to prove the termination of the loop."

    }
    return renderSection(/*html*/`<span style="color: var(--vscode-editorWarning-foreground)">Warning</span>`, sectionBody, `at ${renderRange(warning.range)}`, warning.tag)
}