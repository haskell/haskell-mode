# For Emacs 25.x
FROM phusion/baseimage

USER root

ENV BRANCH

RUN apt-get update \
    && apt-get install -y build-essential \
    && apt-get install -y emacs-nox \
    && apt-get install -y ghc \
    && apt-get install -y git \
    && git clone https://github.com/vasanthaganeshk/haskell-interactive-mode \
    && git checkout $(BRANCH)

WORKDIR ./haskell-interactive-mode

CMD [ "make", "check" ]
