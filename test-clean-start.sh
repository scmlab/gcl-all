#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" || exit 1

IMG="ghcr.io/scmlab/gcl-all-builder-deps-bin"
docker ps -a --filter "ancestor=$IMG:latest" -q | xargs -r docker rm -f
docker tag "$IMG:latest" "$IMG:backup"
docker rmi "$IMG:latest"

D=/tmp/tmp-gcl-all
rm -fR "$D" && mkdir -p "$D"
code --user-data-dir "$D/u" --extensions-dir "$D/e" .
