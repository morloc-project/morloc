#!/bin/sh
printf '%s\n' "$*" | sed 's|--root [^ ]*|--root <dir>|' >> "$HOOK_LOG"
