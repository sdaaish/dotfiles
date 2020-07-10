FROM ubuntu:18.04 AS build

RUN apt-get update --yes && \
  apt-get install --yes --no-install-recommends ca-certificates git make stow && \
  rm -rf /var/lib/apt/lists/*

FROM build
COPY . /src
WORKDIR /src
RUN make
#  source ~/.profile && \
#  install-emacs-d

