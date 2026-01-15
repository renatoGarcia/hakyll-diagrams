FROM alpine:latest

RUN apk update && \
    apk add \
    binutils-gold \
    curl \
    gcc \
    g++ \
    gmp-dev \
    libc-dev \
    libffi-dev \
    make \
    musl-dev \
    ncurses-dev \
    perl \
    tar \
    xz \
    zlib-dev \
    git

RUN curl -L https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup -o /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

ENV PATH="/github/home/.ghcup/bin:$PATH"
