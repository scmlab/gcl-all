const assert = require("node:assert/strict");
const path = require("node:path");
const test = require("node:test");
const vm = require("node:vm");
const { buildSync } = require("esbuild");

const source = buildSync({
  entryPoints: [path.join(__dirname, "../src/extension.ts")],
  bundle: true,
  platform: "node",
  format: "cjs",
  write: false,
  external: ["vscode", "./client"],
}).outputFiles[0].text;

function state(proofExists = false) {
  return {
    errors: [],
    holes: [],
    specs: [],
    warnings: [],
    globalProps: [],
    pos: [
      {
        pred: "x = 0",
        reducedPred: "x = 0",
        strippedPred: "x = 0",
        proofExists,
        smtResult: "Timeout",
        footPrints: [],
      },
    ],
  };
}

async function harness() {
  let update, receive, changeTabs, closeDocument;
  let html = "";
  const dom = { disabled: false, replacements: 0 };
  const errors = [];
  const disposable = { dispose() {} };
  const document = {
    uri: { fsPath: "test.gcl", scheme: "file" },
    fileName: "test.gcl",
    languageId: "gcl",
    version: 1,
    lineCount: 1,
    lineAt: () => ({ range: { end: {} } }),
  };
  let editResult = true,
    editCount = 0,
    focusError = false,
    editWait;
  const editor = {
    document,
    viewColumn: 1,
    async edit(build) {
      editCount++;
      if (editResult instanceof Error) throw editResult;
      if (!editResult) return false;
      build({ insert() {} });
      document.version++;
      await editWait;
      return true;
    },
  };
  const webview = {
    cspSource: "vscode-webview:",
    asWebviewUri: (uri) => uri,
    onDidReceiveMessage: (handler) => {
      receive = handler;
      return disposable;
    },
    get html() {
      return html;
    },
    set html(value) {
      // Match VS Code's HTML string comparison before replacing the DOM.
      if (value === html) return;
      html = value;
      dom.replacements++;
      dom.disabled = /<button class="proof"[^>]*\bdisabled/.test(value);
    },
  };
  const window = {
    activeTextEditor: editor,
    visibleTextEditors: [editor],
    createWebviewPanel: () => ({ webview }),
    tabGroups: {
      onDidChangeTabs: (handler) => {
        changeTabs = handler;
        return disposable;
      },
    },
    onDidChangeActiveTextEditor: () => disposable,
    createOutputChannel: () => ({ ...disposable, appendLine() {} }),
    async showTextDocument() {
      if (focusError) throw new Error("focus failed");
    },
    showErrorMessage: (message) => errors.push(message),
  };
  const vscode = {
    window,
    workspace: {
      onDidCloseTextDocument: (handler) => {
        closeDocument = handler;
        return disposable;
      },
    },
    languages: { registerInlayHintsProvider: () => disposable },
    commands: { executeCommand() {}, registerCommand: () => disposable },
    EventEmitter: class {
      event() {}
      fire() {}
    },
    ViewColumn: { Two: 2 },
    Uri: { joinPath: (...parts) => parts.join("/") },
  };
  const client = {
    async start() {},
    async stop() {},
    async restart() {},
    async sendRequest() {},
    onFileStateNotification: (handler) => {
      update = handler;
      return disposable;
    },
  };
  const module = { exports: {} };
  vm.runInNewContext(source, {
    module,
    exports: module.exports,
    console: { log() {} },
    require: (name) =>
      name === "vscode" ? vscode : name === "./client" ? client : require(name),
  });
  await module.exports.activate({
    extensionUri: "extension",
    extensionPath: "extension",
    subscriptions: [],
  });

  return {
    dom,
    document,
    errors,
    window,
    get html() {
      return html;
    },
    get editCount() {
      return editCount;
    },
    set editResult(value) {
      editResult = value;
    },
    set focusError(value) {
      focusError = value;
    },
    set editWait(value) {
      editWait = value;
    },
    update: (
      isLoad,
      fileState = state(),
      version = document.version,
      filePath = "test.gcl",
    ) =>
      update({
        ...fileState,
        filePath,
        loadDocumentVersion: isLoad ? version : null,
      }),
    proof: () => receive({ action: "proof", pred: "x = 0" }),
    tab: (filePath) =>
      changeTabs({ changed: [{ input: { uri: { fsPath: filePath } } }] }),
    close: () => closeDocument(document),
  };
}

test("identical load results keep the correct button state without replacing the panel", async () => {
  const h = await harness();
  await h.update(true);
  const html = h.html;
  const replacements = h.dom.replacements;
  await h.update(false);
  assert.equal(h.html, html);
  assert.equal(h.dom.replacements, replacements);
  assert.equal(h.dom.disabled, false);
  await h.update(true);
  assert.equal(h.html, html);
  assert.equal(h.dom.replacements, replacements);
  assert.equal(h.dom.disabled, false);
});

test("deleting an insertion before the first reload restores the original enabled HTML", async () => {
  const h = await harness();
  await h.update(true);
  const original = h.html;
  await h.proof();
  assert.equal(h.dom.disabled, true);
  h.document.version++; // Undo or delete the block before loading it.
  await h.update(false);
  assert.equal(h.dom.disabled, true);
  await h.update(true);
  assert.equal(h.html, original);
  assert.equal(h.dom.disabled, false);
});

test("inserted proofs stay disabled across changed didChange HTML until deletion is loaded", async () => {
  const h = await harness();
  await h.update(true);
  await h.proof();
  assert.equal(h.dom.disabled, true);
  const moved = state();
  moved.pos[0].origin = {
    start: { line: 1, character: 0 },
    end: { line: 1, character: 1 },
  };
  await h.update(false, moved);
  assert.equal(h.dom.disabled, true);
  await h.proof();
  assert.equal(h.editCount, 1);
  h.document.version++; // Delete the inserted block.
  await h.update(true);
  assert.equal(h.dom.disabled, false);
});

test("loading an existing block keeps its Proof button disabled", async () => {
  const h = await harness();
  await h.update(true);
  await h.proof();
  await h.update(true, state(true));
  assert.equal(h.dom.disabled, true);
  h.document.version++;
  await h.update(true, state(false));
  assert.equal(h.dom.disabled, false);
});

test("an older load cannot acknowledge a newer insertion", async () => {
  const h = await harness();
  await h.update(true);
  await h.proof();
  await h.update(true, state(), 1);
  assert.equal(h.dom.disabled, true);
  await h.update(true, state(), h.document.version);
  assert.equal(h.dom.disabled, false);
});

test("an in-flight edit stays disabled and rejects duplicate requests", async () => {
  const h = await harness();
  await h.update(true);
  let finishEdit;
  h.editWait = new Promise((resolve) => {
    finishEdit = resolve;
  });
  const insertion = h.proof();
  assert.equal(h.dom.disabled, true);
  await h.proof();
  assert.equal(h.editCount, 1);
  await h.update(true, state(), 1);
  assert.equal(h.dom.disabled, true);
  finishEdit();
  await insertion;
  await h.update(false);
  assert.equal(h.dom.disabled, true);
  await h.update(true);
  assert.equal(h.dom.disabled, false);
});

test("a failed load does not discard an unconfirmed insertion", async () => {
  const h = await harness();
  await h.update(true);
  await h.proof();
  const failed = state();
  failed.pos = [];
  failed.errors = [{ tag: "Others", title: "Load", message: "bad source" }];
  await h.update(true, failed);
  assert.match(h.html, /bad source/);
  await h.proof(); // A queued request from the previous page must still be rejected.
  assert.equal(h.editCount, 1);
  h.document.version++;
  await h.update(true);
  assert.equal(h.dom.disabled, false);
});

for (const failure of [false, new Error("edit failed")]) {
  test(`an unsuccessful edit restores the button (${String(failure)})`, async () => {
    const h = await harness();
    await h.update(true);
    h.editResult = failure;
    await h.proof();
    assert.equal(h.dom.disabled, false);
    h.editResult = true;
    await h.proof();
    assert.equal(h.editCount, 2);
    assert.equal(h.dom.disabled, true);
  });
}

test("a focus failure does not discard a successfully inserted proof", async () => {
  const h = await harness();
  await h.update(true);
  h.focusError = true;
  await h.proof();
  assert.equal(h.errors.length, 1);
  assert.equal(h.dom.disabled, true);
  await h.proof();
  assert.equal(h.editCount, 1);
});

test("pending predicates are isolated by file and survive tab changes", async () => {
  const h = await harness();
  await h.update(true);
  await h.proof();
  await h.update(true, state(), 1, "other.gcl");
  assert.equal(h.dom.disabled, false);
  h.tab("test.gcl");
  assert.equal(h.dom.disabled, true);
  h.close();
  await h.update(true, state(), 1);
  assert.equal(h.dom.disabled, false);
});

test("a missing editor leaves the enabled button and panel unchanged", async () => {
  const h = await harness();
  await h.update(true);
  const replacements = h.dom.replacements;
  h.window.activeTextEditor = undefined;
  h.window.visibleTextEditors = [];
  await h.proof();
  assert.equal(h.editCount, 0);
  assert.equal(h.dom.disabled, false);
  assert.equal(h.dom.replacements, replacements);
});
