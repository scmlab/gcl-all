#!/usr/bin/env bash
set -eu

umask 077
log_dir="${GCL_TRACE_DIR:-/tmp/gcl-trace}"
mkdir -p -- "$log_dir"

# PATH_TO_GCL=/home/vscode/.local/bin/gcl
PATH_TO_GCL=gcl

exec "$PATH_TO_GCL" "$@" \
  < <(tee -a "$log_dir/stdin.log") \
  > >(tee -a "$log_dir/stdout.log") \
  2> >(tee -a "$log_dir/stderr.log" >&2)
