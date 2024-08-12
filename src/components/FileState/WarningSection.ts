
import { IStructWarning } from '../../data/FileState'
import { renderRange } from '../Range'
import renderSection from '../Section'

export default function renderWarning(warning: IStructWarning) {
    return renderSection(/*html*/`<span style="color: var(--vscode-editorWarning-foreground)">Warning</span>`, `${warning.tag} at ${renderRange(warning.range)}`)
}