# For Emacs snapshot
FROM ubuntu:xenial

USER root

RUN apt-get update \
    && apt-get install -y software-properties-common \
    && add-apt-repository -y ppa:ubuntu-elisp/ppa \
    && apt-get update \
    && sudo apt-get install --no-install-suggests --no-install-recommends -y emacs-snapshot \
    && sudo apt-get install -y ghc \
    && apt-get install -y git \
    && git clone https://github.com/vasanthaganeshk/haskell-interactive-mode

WORKDIR ./haskell-interactive-mode

CMD [ "make", "check" ]