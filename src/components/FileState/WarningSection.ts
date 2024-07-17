
import { IStructWarning } from '../../data/FileState'
import { renderRange } from '../Range'
import renderSection from '../Section'

export default function renderWarning(warning: IStructWarning) {
    return renderSection("Warning", `${warning.tag} at ${renderRange(warning.range)}`)
}