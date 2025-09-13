FROM ubuntu:24.04 AS build

RUN apt-get update --yes && \
    apt-get install --yes --no-install-recommends ca-certificates git curl && \
    rm -rf /var/lib/apt/lists/*

FROM build

ARG GITHUB_USERNAME

COPY . /src
WORKDIR /src
RUN /src/setup.sh
