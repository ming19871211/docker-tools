FROM openjdk:8-jre-alpine AS build

# Install required packages
RUN apk add --no-cache \
    bash \
    python \
    su-exec
ENV SKYWALKING_HOME=/skywalking SKYWALKING_VERSION=7.0.0

ENV SKYWALKING_TGZ_URL=https://www.apache.org/dyn/closer.cgi?action=download&filename=skywalking/$SKYWALKING_VERSION/apache-skywalking-apm-es7-$SKYWALKING_VERSION.tar.gz
#ENV SKYWALKING_TGZ_URL=https://mirror.bit.edu.cn/apache/skywalking/$SKYWALKING_VERSION/apache-skywalking-apm-es7-$SKYWALKING_VERSION.tar.gz
RUN mkdir -p "${SKYWALKING_HOME}"
WORKDIR $SKYWALKING_HOME

RUN set -eux; \
    wget -O skywalking.tar.gz "$SKYWALKING_TGZ_URL"; \
    tar -xvf skywalking.tar.gz --strip-components=1; \
    rm skywalking.tar.gz*;

FROM busybox:latest
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>
COPY --from=build /skywalking/agent /skywalking/agent
