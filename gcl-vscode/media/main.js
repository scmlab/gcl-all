const vscode = acquireVsCodeApi();
const buttons = document.querySelectorAll(".clickable");

function handleClick(event) {
  event.stopPropagation();

  const id = event.target.dataset.redexId;

  vscode.postMessage({
    id: id,
  });
}

for (const btn of buttons) {
  btn.addEventListener("click", handleClick);
}
