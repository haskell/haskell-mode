# For Emacs snapshot
FROM ubuntu:latest

USER root

RUN apt-get update \
    && apt-get install -y build-essential \
    && apt-get install -y software-properties-common \
    && add-apt-repository -y ppa:ubuntu-elisp/ppa \
    && apt-get update \
    && apt-get install --no-install-suggests --no-install-recommends -y emacs-snapshot \
    && sudo add-apt-repository ppa:hvr/ghc \
    && sudo apt-get install ghc-8.0.2 \
    && sudo apt-get update \
    && apt-get install -y git \
    && git clone https://github.com/vasanthaganeshk/haskell-interactive-mode

WORKDIR ./haskell-interactive-mode

CMD [ "make", "check" ]
