# SPDX-License-Identifier: MIT AND LicenseRef-Palimpsest-0.8
#
# Containerfile for STATE - Wolfi base with Guix
#
# Build:
#   podman build -t state:latest .
#   nerdctl build -t state:latest .
#
# Run:
#   podman run -it --rm state:latest
#   nerdctl run -it --rm state:latest
#
# Development (mount workspace):
#   podman run -it --rm -v $(pwd):/workspace:Z state:latest
#   nerdctl run -it --rm -v $(pwd):/workspace state:latest

FROM cgr.dev/chainguard/wolfi-base:latest

LABEL org.opencontainers.image.title="STATE"
LABEL org.opencontainers.image.description="Stateful Context Tracking Engine for AI Conversation Continuity"
LABEL org.opencontainers.image.version="2.0.0"
LABEL org.opencontainers.image.licenses="MIT AND LicenseRef-Palimpsest-0.8"
LABEL org.opencontainers.image.source="https://github.com/Hyperpolymath/STATE.scm"

# Install minimal dependencies needed for Guix
# Wolfi uses apk package manager
USER root
RUN apk add --no-cache \
    bash \
    curl \
    xz \
    shadow \
    && rm -rf /var/cache/apk/*

# Create guix build group and user
RUN groupadd --system guixbuild && \
    for i in $(seq -w 1 10); do \
      useradd -g guixbuild -G guixbuild \
              -d /var/empty -s /sbin/nologin \
              -c "Guix build user $i" --system \
              "guixbuilder$i"; \
    done

# Create state user
RUN useradd -m -s /bin/bash state

# Install Guix daemon and binary
RUN curl -fsSL "https://ftp.gnu.org/gnu/guix/guix-binary-1.4.0.x86_64-linux.tar.xz" \
    -o /tmp/guix.tar.xz && \
    cd /tmp && tar xf guix.tar.xz && \
    mv var/guix /var/ && mv gnu /gnu && \
    rm -rf /tmp/guix.tar.xz /tmp/var /tmp/gnu && \
    mkdir -p ~root/.config/guix && \
    ln -sf /var/guix/profiles/per-user/root/current-guix ~root/.config/guix/current

# Set up Guix paths
ENV PATH="/root/.config/guix/current/bin:${PATH}"
ENV GUIX_LOCPATH="/root/.guix-profile/lib/locale"

# Authorize Guix substitutes
RUN guix archive --authorize < /gnu/store/*-guix-*/share/guix/ci.guix.gnu.org.pub || true

# Switch to state user for package installation
USER state
WORKDIR /home/state

# Set up user Guix profile
ENV GUIX_PROFILE="/home/state/.guix-profile"
ENV PATH="${GUIX_PROFILE}/bin:/var/guix/profiles/per-user/root/current-guix/bin:${PATH}"

# Copy manifest for package installation
COPY --chown=state:state manifest.scm ./

# Install packages via Guix (guile, graphviz)
# This uses substitutes from ci.guix.gnu.org for fast binary installs
RUN guix package -m manifest.scm || \
    guix install guile guile-minikanren graphviz || \
    echo "Guix package install incomplete - may need daemon"

# Copy STATE library files
COPY --chown=state:state lib/ ./lib/
COPY --chown=state:state STATE.scm ./
COPY --chown=state:state examples/ ./examples/

# Set up Guile load path
ENV GUILE_LOAD_PATH="/home/state/lib:${GUILE_LOAD_PATH}"

# Default command: start REPL with STATE loaded
CMD ["guile", "-L", "lib", "-l", "lib/state.scm"]
