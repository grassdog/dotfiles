#!/bin/bash

# Create a fresh Ubuntu instance tied to your current working directory

set -euo pipefail

CONTAINER="$(pwd | sha256sum | awk '{print $1}')"
if docker inspect "$CONTAINER" 2> /dev/null > /dev/null;
then
  docker start "$CONTAINER" -ai
else
  docker run --name="$CONTAINER" -v $(pwd):/root -w /root -ti ubuntu bash
fi
