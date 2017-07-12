# For Emacs 24.x
FROM ubuntu:xenial

USER root

WORKDIR /opt/haskell-interactive-mode

RUN apt-get update \
    && apt-get install --no-install-suggests --no-install-recommends --force-yes emacs24-nox \
    && apt-get install -y ghc \
    && apt-get install -y git \
    && git clone https://github.com/vasanthaganeshk/haskell-interactive-mode

CMD [ "make", "check" ]