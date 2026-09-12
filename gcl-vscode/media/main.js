const vscode = acquireVsCodeApi();

/**
 * @type {EventListener}
 */
function handleReduce(event) {
  event.stopPropagation();

  const el = event.target;

  const redex = el.dataset.redex.split(",").map(Number);
  const po = Number(el.closest(".gcl-expr").dataset.po);

  vscode.postMessage({
    action: "reduce",
    po,
    redex,
  });
}

const redexes = document.querySelectorAll(".gcl-redex");

for (const redex of redexes) {
  redex.addEventListener("click", handleReduce);
}

/**
 * @type {EventListener}
 */
function handleProof(event) {
  event.stopPropagation();

  const el = event.target;

  const pred = el.dataset.pred;

  vscode.postMessage({
    action: "proof",
    pred,
  });
}

const proofes = document.querySelectorAll(".proof");

for (const proof of proofes) {
  proof.addEventListener("click", handleProof);
}
