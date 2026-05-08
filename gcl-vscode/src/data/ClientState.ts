import { ClientFileState } from "./FileState"

export type FileStateNotification = { filePath: string } & ClientFileState;
