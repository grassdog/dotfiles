#!/usr/bin/env bash
set -e
set -u
set -o pipefail

if [ ! $# -eq 1 ]; then
  echo 'mksh takes one argument' 1>&2
  exit 1
elif [ -e "$1" ]; then
  echo "$1 already exists" 1>&2
  exit 1
fi

echo '#!/usr/bin/env bash
set -e
set -u
set -o pipefail

' > "$1"

chmod u+x "$1"

"$EDITOR" "$1"
