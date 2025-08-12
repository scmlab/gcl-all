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
3. Open a ".hs" file in gcl/src and click "Manually via PATH" (*NOT* "Automatically via GHCup" !)
4. Open a vscode terminal in gcl/ and run "stack install" to update ~/.local/bin/gcl
5. Press F5 to launch the extension + LSP server in a new window


## Codespaces
1. Select "Code -> Codespaces -> Create codespace on main" and wait for 5 to 10 minutes. If it hangs, delete it and start over again.
2. Click "Open Workspace", or open "gcl-all.code-workspace" and click "Open Workspace"
3. Open a ".hs" in gcl/src, click "Manually via PATH" (*NOT* "Automatically via GHCup" !)
4. Delete the codespace at https://github.com/codespaces
