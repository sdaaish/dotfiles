FROM ubuntu:latest AS build

RUN apt-get update --yes && \
  apt-get update --yes && \
  apt-get install --yes --no-install-recommends ca-certificates git make

FROM build
COPY . /src
WORKDIR /src
RUN make
#  source ~/.profile && \
#  install-emacs-d

