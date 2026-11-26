import { ClientFileState } from "./data/FileState";

// Keep successful insertions disabled until a load includes their source version.
// A null version means the editor edit is still in flight.
export class ProofState {
  private pending = new Map<string, Map<string, number | null>>();
  private loadVersions = new Map<string, number>();

  begin(filePath: string, pred: string): boolean {
    let proofs = this.pending.get(filePath);
    if (!proofs) {
      proofs = new Map();
      this.pending.set(filePath, proofs);
    }
    if (proofs.has(pred)) return false;
    proofs.set(pred, null);
    return true;
  }

  inserted(filePath: string, pred: string, version: number): void {
    const proofs = this.pending.get(filePath);
    if (!proofs?.has(pred)) return;
    if ((this.loadVersions.get(filePath) ?? -1) >= version) {
      proofs.delete(pred);
    } else {
      proofs.set(pred, version);
    }
  }

  cancel(filePath: string, pred: string): void {
    this.pending.get(filePath)?.delete(pred);
  }

  loaded(filePath: string, version: number): void {
    this.loadVersions.set(
      filePath,
      Math.max(version, this.loadVersions.get(filePath) ?? -1),
    );
    for (const [pred, insertedVersion] of this.pending.get(filePath) ?? []) {
      if (insertedVersion !== null && insertedVersion <= version) {
        this.cancel(filePath, pred);
      }
    }
  }

  // Overlay pending insertions for rendering without changing the server cache.
  forDisplay(filePath: string, state: ClientFileState): ClientFileState {
    const proofs = this.pending.get(filePath);
    if (!proofs?.size) return state;
    return {
      ...state,
      pos: state.pos.map((po) => ({
        ...po,
        proofExists: Boolean(po.proofExists) || proofs.has(po.strippedPred),
      })),
    };
  }

  forget(filePath: string): void {
    this.pending.delete(filePath);
    this.loadVersions.delete(filePath);
  }
}
