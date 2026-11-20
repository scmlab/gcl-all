import { ClientFileState } from "./FileState";

export type FileStateNotification = {
  filePath: string;
  // The source version checked by load; null for other updates.
  loadDocumentVersion: number | null;
} & ClientFileState;
