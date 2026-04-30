#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <words-file>" >&2
  exit 1
fi

words_file="$1"

if [[ ! -f "$words_file" ]]; then
  echo "Error: file not found: $words_file" >&2
  exit 1
fi

echo "filtering 4-letters or more words from dictionary ..."

LC_ALL=C grep -E '^[a-z]{4,}$' "$words_file" \
  | grep -Ev '^m{0,3}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})$' \
  | sort -u > dictionary

echo "$(wc -l < dictionary) words in dictionary"
