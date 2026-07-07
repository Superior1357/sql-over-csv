#!/usr/bin/env bash
set -euo pipefail

binary_path="$1"

rm -f students users more_users combined shared unique_users

run_cmd() {
  local cmd="$1"
  "$binary_path" noninteractive -c "$cmd"
}

while IFS= read -r cmd; do
  echo "Running command: $cmd"
  run_cmd "$cmd"
  read -p "Press enter to continue..." choice < /dev/tty
done < "example_commands"

echo "Example script completed successfully."
