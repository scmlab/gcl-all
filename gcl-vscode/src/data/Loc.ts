import { Range } from "vscode-languageclient";

export type Loc = Range & { filePath: string }