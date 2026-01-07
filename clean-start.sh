#!/bin/bash
set -e
cd "$(dirname "${BASH_SOURCE[0]}")"

# Check if 'code' command exists
if ! command -v code &> /dev/null; then
    echo "Error: VS Code CLI 'code' is not installed or not in PATH."
    echo "Please install VS Code and ensure the 'code' command is available."
    exit 1
fi

echo "This script helps you start a clean VS Code instance."

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

echo "Creating temporary VS Code user data and extensions dirs..."
TEMP_DIR=$(mktemp -d)
echo "Temporary directory created at: $TEMP_DIR"
echo

echo "Pre-installing the Dev Containers extension ... (to avoid the prompt)"
code --user-data-dir "$TEMP_DIR/u" --extensions-dir "$TEMP_DIR/e"  --install-extension ms-vscode-remote.remote-containers
echo

echo "Launching VS Code with clean user data and extensions dirs ..."
echo


# Launch Option 1:
# Simply open the current folder, then choose "Reopen in Container"
#
#echo "Choose 'Reopen in Container' from the Command Palette to open the current folder in the devcontainer."
#echo
#code --user-data-dir "$TEMP_DIR/u" --extensions-dir "$TEMP_DIR/e" .


# Launch Option 2:
# In VS Code, run the ">Dev Containers: Install devcontainer CLI" command
# Then use it to open the folder in the devcontainer


# Launch Option 3:
# Directly open the current folder in the devcontainer
#
# This script tries to mimic what VS Code does when you choose "Reopen in Container".
# If your devcontainer.json defines a custom workspaceFolder or workspaceMount,
# the generated URI will be incorrect, and VS Code will open the wrong path inside the container.
#
echo "Entering the devcontainer directly ..."
echo
URI=$(./get-devcontainer-uri.sh)
if [ -z "$URI" ]; then
    echo "Error: Failed to generate Dev Container URI."
    exit 1
fi
echo "Generated URI: $URI"
#code --user-data-dir "$TEMP_DIR/u" --extensions-dir "$TEMP_DIR/e" --folder-uri="$URI"

# Determine if we should open a workspace file or the folder
WORKSPACE_FILE=$(ls *.code-workspace 2>/dev/null | head -n 1)
FLAG="--folder-uri"
if [ -n "$WORKSPACE_FILE" ]; then
    URI="${URI}/${WORKSPACE_FILE}"
    FLAG="--file-uri"
fi

echo "Opening $FLAG: $URI"
code --user-data-dir "$TEMP_DIR/u" --extensions-dir "$TEMP_DIR/e" "$FLAG" "$URI"
