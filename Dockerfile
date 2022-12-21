# AGC Toolchain

FROM alpine:3.17 as toolchain

RUN apk update
RUN apk add build-base ncurses-dev musl-dev

ADD blobs/virtualagc-20221005.tar.gz /tmp

RUN cd /tmp/virtualagc-20221005 && make yaAGC yaYUL 2>/dev/null

# Application

FROM rust:1.66-alpine as application

RUN apk add musl-dev

COPY --from=toolchain /tmp/virtualagc-20221005/yaAGC/yaAGC /usr/local/bin
COPY --from=toolchain /tmp/virtualagc-20221005/yaYUL/yaYUL /usr/local/bin

WORKDIR /tmp

COPY Cargo.toml Cargo.lock moonshot/
COPY .cargo moonshot/.cargo/
COPY src moonshot/src/

RUN cd moonshot && cargo install --path .
RUN moonshot
