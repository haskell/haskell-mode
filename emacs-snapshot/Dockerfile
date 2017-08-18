# For Emacs snapshot
FROM ubuntu:latest

USER root

RUN apt-get update \
    && apt-get install -y build-essential \
    && apt-get install -y software-properties-common \
    && add-apt-repository -y ppa:ubuntu-elisp/ppa \
    && apt-get update \
    && apt-get install --no-install-suggests --no-install-recommends -y emacs-snapshot \
    && apt-get install -y ghc

WORKDIR /root/haskell-mode

CMD [ "make", "check" ]
