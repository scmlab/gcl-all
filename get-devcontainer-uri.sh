#!/bin/sh

# -------------------------------------------------------------------------------------------------------------
# CLI Utility: Get Dev Container URI
#
# This script calculates the 'vscode-remote://' URI for opening the current folder in a Dev Container.
#
# Logic Alignment with TypeScript Source:
# 1. findGitRootFolder (src/spec-common/git.ts):
#    - Implements the logic to traverse up the directory tree looking for '.git/config'.
#    - Used to determine the project root.
# 2. getHostMountFolder (src/spec-node/utils.ts):
#    - Uses the found git root (or current dir) as the folder to be mounted from the host.
#
# Limitations / Missing Logic vs TypeScript:
# 1. getWorkspaceConfiguration (src/spec-node/utils.ts):
#    - This script DOES NOT parse 'devcontainer.json'.
#    - It fails to detect custom "workspaceFolder" or "workspaceMount" properties.
#    - It assumes the default container workspace path: /workspaces/<folder_name>
#    - To fix this, a JSON parser (like jq or node) is required to handle JSONC (comments) in devcontainer.json.
# -------------------------------------------------------------------------------------------------------------

# Function to hex encode a string using POSIX tools (od, tr, printf)
hex_encode() {
    # od -An: no address
    # -v: don't skip duplicate lines
    # -tx1: output as hex bytes
    # tr -d ' \n': remove spaces and newlines
    printf '%s' "$1" | od -An -v -tx1 | tr -d ' \n'
}

# Get current directory
current_dir=$(pwd)

# Default to current directory assuming no git root
host_path="$current_dir"
found_git_root=0

# Traverse up directory tree to find .git/config
# Logic matches src/spec-common/git.ts findGitRootFolder (fallback w/o exec):
# It checks ONLY for .git/config, not just .git (so it might miss worktrees if they lack config in place, matching TS behavior)
dir="$current_dir"
while [ "$dir" != "/" ] && [ "$dir" != "." ]; do
    if [ -f "$dir/.git/config" ]; then
        host_path="$dir"
        found_git_root=1
        break
    fi
    dir=$(dirname "$dir")
done

# Check root directory case if loop terminated
if [ "$found_git_root" -eq 0 ]; then
    if [ -f "/.git/config" ]; then
        host_path="/"
        found_git_root=1
    fi
fi

# Calculate relative path manually using POSIX string manipulation
if [ "$host_path" = "/" ]; then
    # If host is root, remove leading slash from current_dir
    relative_path="${current_dir#/}"
else
    # Remove host_path prefix from current_dir
    relative_path="${current_dir#$host_path}"
    # Remove leading slash if it exists
    relative_path="${relative_path#/}"
fi

# Determine the workspace folder name (basename of the host path)
workspace_folder_name=$(basename "$host_path")
if [ "$workspace_folder_name" = "/" ] || [ "$workspace_folder_name" = "." ]; then
    workspace_folder_name="root"
fi

# Construct the container workspace path (/workspaces/<folder_name>)
container_base_path="/workspaces/$workspace_folder_name"

if [ -n "$relative_path" ]; then
    container_work_dir="${container_base_path}/${relative_path}"
else
    container_work_dir="${container_base_path}"
fi

# Hex encode the current directory path (assuming it contains .devcontainer)
hex_host_path=$(hex_encode "$current_dir")

# Construct the full URI
printf 'vscode-remote://dev-container+%s%s\n' "$hex_host_path" "$container_work_dir"

