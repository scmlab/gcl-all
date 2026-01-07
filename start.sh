#!/bin/bash
set -e
cd "$(dirname "${BASH_SOURCE[0]}")"

# Check if 'code' command exists
if ! command -v code &> /dev/null; then
    echo "Error: VS Code CLI 'code' is not installed or not in PATH."
    echo "Choose \">Shell Command: Install 'code' command in PATH\" from the Command Palette to install it."
    exit 1
fi

# Cleanup existing Dev Containers for this project to ensure a truly clean start
if command -v docker &> /dev/null; then
    echo "Cleaning up existing Dev Containers for this project..."
    CURRENT_DIR="$(pwd)"
    # Filter by 'devcontainer.local_folder' which is the current label for Dev Containers
    CONTAINER_IDS=$(docker ps -aq --filter "label=devcontainer.local_folder=$CURRENT_DIR")

    if [ -n "$CONTAINER_IDS" ]; then
        echo "Found existing containers, removing: $CONTAINER_IDS"
        docker rm -f $CONTAINER_IDS
    else
        echo "No existing containers found for this project."
    fi
    echo
fi

HEX_LOCAL_WORKSPACE_FOLDER=$(printf '%s' `pwd` | od -An -v -tx1 | tr -d ' \n')
BASE_NAME=$(basename $(pwd))
FILE_URI="vscode-remote://dev-container+$HEX_LOCAL_WORKSPACE_FOLDER/workspaces/$BASE_NAME/gcl-all.code-workspace"

if [[ "$1" == "clean" ]]; then
    echo "Creating temporary VS Code user data and extensions dirs..."
    TEMP_DIR=$(mktemp -d)
    echo "Temporary directory created at: $TEMP_DIR"
    echo
    echo "Pre-installing the Dev Containers extension ..." # to avoid the prompt
    code --user-data-dir "$TEMP_DIR/u" --extensions-dir "$TEMP_DIR/e" --install-extension ms-vscode-remote.remote-containers
    echo
    code --user-data-dir "$TEMP_DIR/u" --extensions-dir "$TEMP_DIR/e" --file-uri="$FILE_URI"
else
    code --file-uri="$FILE_URI"
fi
