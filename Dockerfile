# ========== AGC Toolchain ==========

FROM alpine:3.17 as toolchain

RUN apk update
RUN apk add build-base ncurses-dev musl-dev

WORKDIR /tmp

RUN wget https://github.com/virtualagc/virtualagc/archive/refs/tags/20221005.tar.gz -O virtualagc.tar.gz
RUN ls -l /tmp
RUN tar -xvf virtualagc.tar.gz
RUN cd virtualagc-20221005 && make yaAGC yaYUL 2>/dev/null

# ========== Application ==========

FROM rust:1.76-alpine as application

RUN apk add musl-dev

COPY --from=toolchain /tmp/virtualagc-20221005/yaAGC/yaAGC /usr/local/bin
COPY --from=toolchain /tmp/virtualagc-20221005/yaYUL/yaYUL /usr/local/bin

WORKDIR /tmp

COPY Cargo.toml Cargo.lock moonshot/
COPY build.rs moonshot/
COPY src moonshot/src/
COPY tests moonshot/tests/
COPY examples moonshot/examples/

# Trigger a build to cache dependencies
RUN cd /tmp/moonshot && cargo build

# Run tests
ENV MOONSHOT_RUN_INTEGRATION_TESTS=1
RUN cd /tmp/moonshot && cargo test
