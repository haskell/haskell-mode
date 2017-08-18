# For Emacs 25.x
FROM phusion/baseimage

USER root

RUN apt-get update \
    && apt-get install -y build-essential \
    && apt-get install -y emacs-nox \
    && apt-get install -y ghc \
    && apt-get install -y git \
    && git clone https://github.com/vasanthaganeshk/haskell-interactive-mode

WORKDIR ./haskell-interactive-mode

ENV BRANCH=$BRANCH

ENTRYPOINT ["/bin/bash", "-c", "git checkout $BRANCH"]

CMD make check
