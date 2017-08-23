# For Emacs 25.x
FROM ubuntu:rolling

USER root

RUN apt-get update \
    && apt-get install -y build-essential \
    && apt-get install -y emacs-nox \
    && apt-get install -y ghc \
    && apt-get install -y haskell-stack

WORKDIR /root/haskell-mode

CMD [ "make", "check" ]
