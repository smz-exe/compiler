FROM --platform=linux/amd64 ubuntu:22.04

# Avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install OCaml and build tools
RUN apt-get update && apt-get install -y \
    ocaml \
    gcc \
    make \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /work

# Default command
CMD ["bash"]
