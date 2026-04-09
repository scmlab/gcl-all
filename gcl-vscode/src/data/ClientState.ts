import { Error } from "./Error"
import { FileState } from "./FileState"

export interface Errors {
    errors: Error[];
}

export type FileStateNotification = { filePath: string } & FileState & Errors;

export type ClientState = FileState & Errors;
