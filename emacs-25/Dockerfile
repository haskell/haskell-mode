# For Emacs 25.x
FROM ubuntu:latest

USER root

RUN apt-get update \
    && apt-get install -y build-essential \
    && apt-get install -y emacs-nox \
    && apt-get install -y ghc

WORKDIR /root/haskell-interactive-mode

CMD make check
