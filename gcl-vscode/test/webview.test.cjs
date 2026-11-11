const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const test = require("node:test");
const vm = require("node:vm");

const source = fs.readFileSync(
  path.join(__dirname, "../media/main.js"),
  "utf8",
);

function clickRedex(expressionData, redexPath, nested = false) {
  const messages = [];
  let onClick;
  let stopped = false;
  const redex = {
    dataset: { redex: redexPath },
    closest: () => ({ dataset: expressionData }),
    addEventListener: (event, listener) => {
      if (event === "click") onClick = listener;
    },
  };
  vm.runInNewContext(source, {
    acquireVsCodeApi: () => ({
      postMessage: (message) =>
        messages.push(JSON.parse(JSON.stringify(message))),
    }),
    document: {
      querySelectorAll: (selector) =>
        selector === ".gcl-redex" ? [redex] : [],
    },
  });
  onClick({
    currentTarget: redex,
    target: nested ? { dataset: {} } : redex,
    stopPropagation: () => {
      stopped = true;
    },
  });
  assert.equal(stopped, true);
  assert.equal(messages.length, 1);
  return messages[0];
}

test("a root PO redex keeps the empty path and legacy PO request", () => {
  assert.deepEqual(clickRedex({ po: "0" }, ""), {
    action: "reduce",
    po: 0,
    redex: [],
  });
});

test("a nested PO click uses the bound redex rather than its inner location span", () => {
  assert.deepEqual(clickRedex({ po: "2" }, "1,0", true), {
    action: "reduce",
    po: 2,
    redex: [1, 0],
  });
});

for (const tag of ["SpecificationPrecondition", "SpecificationPostcondition"]) {
  test(`a ${tag} click carries its stable specification ID`, () => {
    const target = { tag, contents: 41 };
    assert.deepEqual(clickRedex({ target: JSON.stringify(target) }, "", true), {
      action: "reduce",
      target,
      redex: [],
    });
  });
}
