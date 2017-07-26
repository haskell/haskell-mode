# For Emacs 24.x
FROM ubuntu:latest

USER root

RUN apt-get update \
    && apt-get install -y build-essential \
    && apt-get install --no-install-suggests --no-install-recommends -y emacs24-nox \
    && add-apt-repository ppa:hvr/ghc \
    && apt-get update \
    && apt-get install ghc-8.0.2 \
    && apt-get install -y git \
    && git clone https://github.com/vasanthaganeshk/haskell-interactive-mode

WORKDIR ./haskell-interactive-mode

CMD [ "make", "check" ]
