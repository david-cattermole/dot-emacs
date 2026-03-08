#!/usr/bin/env bash
# build.sh — Byte-compile all Elisp packages in the dot-emacs repo.
#
# Usage:
#   ./build.sh              # compile everything
#   ./build.sh --debug-init # pass extra flags to Emacs
#
# Requires Emacs to be on PATH.  Set EMACS= to override the binary.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMACS="${EMACS:-emacs}"

exec "$EMACS" \
    --batch \
    --no-site-file \
    --no-site-lisp \
    -l "$SCRIPT_DIR/build.el" \
    "$@"
