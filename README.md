# gcl-all

## Local Development
0. (optional) Run test-clean-start.sh
1. Open the project in a dev container
   - install the recommended "Dev Containers" vscode extension
   - allow it to connect to the local Docker daemon
   - reopen the project in the container
2. Open the multi-root workspace
   - select the file "gcl-all.code-workspace"
   - click "Open Workspace"
3. Open a ".hs" file in gcl/src and click "Manually via PATH" (this will set `haskell.manageHLS` to PATH). Important: Do **NOT** select "Automatically via GHCup"!
4. Open a vscode terminal in `gcl/` and run "stack install" to update ~/.local/bin/gcl
5. Press `Ctrl + F5` to launch the extension + LSP server in a new window


## SMT settings
The following VS Code settings can be set in User or Workspace settings:

```json
{
  "gcl-vscode.smtTimeoutMs": 2000,
  "gcl-vscode.smtReductionLimit": 100
}
```

Changes apply on the next GCL Reload without restarting the server. All proof obligations in a load share the time budget; each global property and proof obligation has its own reduction step limit. Reduction and solver startup may add overhead beyond the time budget.

## Codespaces
1. Select "Code -> Codespaces -> Create codespace on main" and wait for 5 to 10 minutes. If it hangs, delete it and start over again.
2. Click "Open Workspace", or open "gcl-all.code-workspace" and click "Open Workspace"
3. Open a ".hs" in gcl/src, click "Manually via PATH" (*NOT* "Automatically via GHCup" !)
4. Delete the codespace at https://github.com/codespaces


## Releasing
1. Bump `version` in `gcl-vscode/package.json`. This — not the tag — sets the `.vsix` version. The Marketplace rejects re-publishing the same version, so it must increase every release.
2. Commit, then tag with the **same** version and push: `git tag v0.42.0 && git push origin main --tags`.
3. A cold build takes ~20–25 min. Verify the release: `gh release view v0.42.0 --json assets -q '.assets[].name'` (expect 4 `.vsix` + 4 binary archives).
