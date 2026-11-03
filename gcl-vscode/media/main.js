const vscode = acquireVsCodeApi();

/**
 * @type {EventListener}
 */
function handleReduce(event) {
  event.stopPropagation();

  const el = event.currentTarget;

  const redex =
    el.dataset.redex === "" ? [] : el.dataset.redex.split(",").map(Number);
  const expression = el.closest(".gcl-expr");
  const target = expression.dataset.target;

  vscode.postMessage({
    action: "reduce",
    ...(target
      ? { target: JSON.parse(target) }
      : { po: Number(expression.dataset.po) }),
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

  event.currentTarget.disabled = true;

  vscode.postMessage({
    action: "proof",
    pred,
  });
}

const proofes = document.querySelectorAll(".proof");

for (const proof of proofes) {
  proof.addEventListener("click", handleProof);
}
