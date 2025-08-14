#!/bin/bash
#
# Purpose of test-clean-start.sh:
# Help Guabao developers create installation and operation procedures.
#
# Strategy:
# - Host environment: Minimal assumptions (no pre-installed Dev Container extension)
#   because we don't control developers' existing setups
# - Container environment: Fully prepared toolchain with all extensions
#   because this environment is under our control

cd "$(dirname "${BASH_SOURCE[0]}")" || exit 1

IMG="ghcr.io/scmlab/gcl-all-builder-deps-bin"
docker ps -a --filter "ancestor=$IMG:latest" -q | xargs -r docker rm -f
docker tag "$IMG:latest" "$IMG:backup"
docker rmi "$IMG:latest"

D=/tmp/tmp-gcl-all
rm -fR "$D" && mkdir -p "$D"
code --user-data-dir "$D/u" --extensions-dir "$D/e" .
