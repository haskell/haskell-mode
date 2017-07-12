# For Emacs 24.x
FROM phusion/baseimage

USER root

RUN apt-get update \
    && apt-get install -y build-essential \
    && apt-get install --no-install-suggests --no-install-recommends -y emacs24-nox \
    && apt-get install -y ghc \
    && apt-get install -y git \
    && git clone https://github.com/vasanthaganeshk/haskell-interactive-mode

WORKDIR ./haskell-interactive-mode

CMD [ "make", "check" ]