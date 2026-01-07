#!/bin/bash
set -e
cd "$(dirname "${BASH_SOURCE[0]}")"

# Check if 'code' command exists
if ! command -v code &> /dev/null; then
    echo "Error: VS Code CLI 'code' is not installed or not in PATH."
    echo "Please install VS Code and ensure the 'code' command is available."
    exit 1
fi

echo "Launching VS Code in the devcontainer..."

URI=$(./get-devcontainer-uri.sh)
if [ -z "$URI" ]; then
    echo "Error: Failed to generate Dev Container URI."
    exit 1
fi

# Determine if we should open a workspace file or the folder
WORKSPACE_FILE=$(ls *.code-workspace 2>/dev/null | head -n 1)
FLAG="--folder-uri"
if [ -n "$WORKSPACE_FILE" ]; then
    URI="${URI}/${WORKSPACE_FILE}"
    FLAG="--file-uri"
fi

echo "Opening $FLAG: $URI"
code "$FLAG" "$URI"
