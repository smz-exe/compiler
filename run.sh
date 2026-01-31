#!/bin/bash
# Helper script to run the compiler in Docker (x86_64 Linux)

IMAGE_NAME="spl-compiler"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Build Docker image if it doesn't exist
if ! docker image inspect "$IMAGE_NAME" &> /dev/null; then
    echo "Building Docker image..."
    docker build -t "$IMAGE_NAME" "$SCRIPT_DIR"
fi

# Run command in Docker
# Usage: ./run.sh [command]

if [ $# -eq 0 ]; then
    # Interactive mode
    docker run --rm -it -v "$SCRIPT_DIR:/work" "$IMAGE_NAME" bash
else
    # Run specific command (use -i only if stdin is a terminal)
    if [ -t 0 ]; then
        docker run --rm -it -v "$SCRIPT_DIR:/work" "$IMAGE_NAME" bash -c "$*"
    else
        docker run --rm -v "$SCRIPT_DIR:/work" "$IMAGE_NAME" bash -c "$*"
    fi
fi
