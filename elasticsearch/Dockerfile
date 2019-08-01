FROM alpine:latest
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>
# Update image and install base packages
RUN apk update && \
    apk upgrade && \
    apk add bash curl openjdk8 openssl && \
    rm -rf /var/cache/apk/*

# Install elasticsearch user
RUN adduser -D -u 1000 -h /usr/share/elasticsearch elasticsearch

# docker build -t ming19871211/elasticsearch:2.4.5 --build-arg ELASTIC_VERSION=2.4.5 \
# --build-arg ES_DOWNLOAD_URL=https://download.elastic.co/elasticsearch/release/org/elasticsearch/distribution/tar/elasticsearch/2.4.5  \
# --build-arg ES_JAVA_OPTS="-Xms2g -Xmx2g" .
ARG ELASTIC_VERSION
ARG ES_DOWNLOAD_URL
ARG ES_JAVA_OPTS

ENV PATH /usr/share/elasticsearch/bin:$PATH
ENV JAVA_HOME /usr/lib/jvm/java-1.8-openjdk

WORKDIR /usr/share/elasticsearch

# Download/extract defined ES version. busybox tar can't strip leading dir.
RUN wget ${ES_DOWNLOAD_URL}/elasticsearch-${ELASTIC_VERSION}.tar.gz && \
    EXPECTED_SHA=$(wget -O - ${ES_DOWNLOAD_URL}/elasticsearch-${ELASTIC_VERSION}.tar.gz.sha1) && \
    test $EXPECTED_SHA == $(sha1sum elasticsearch-${ELASTIC_VERSION}.tar.gz | awk '{print $1}') && \
    tar zxf elasticsearch-${ELASTIC_VERSION}.tar.gz && \
    chown -R elasticsearch:elasticsearch elasticsearch-${ELASTIC_VERSION} && \
    mv elasticsearch-${ELASTIC_VERSION}/* . && \
    rmdir elasticsearch-${ELASTIC_VERSION} && \
    rm elasticsearch-${ELASTIC_VERSION}.tar.gz

RUN set -ex && for esdirs in config data logs; do \
        mkdir -p "$esdirs"; \
        chown -R elasticsearch:elasticsearch "$esdirs"; \
    done

USER elasticsearch

# Install xpack
RUN eval ${ES_JAVA_OPTS:-} elasticsearch-plugin install --batch x-pack

COPY elasticsearch.yml config/
COPY log4j2.properties config/
COPY es-docker bin/es-docker

USER root
RUN chown elasticsearch:elasticsearch config/elasticsearch.yml config/log4j2.properties bin/es-docker && \
    chmod 0750 bin/es-docker

USER elasticsearch
CMD ["/bin/bash", "bin/es-docker"]

EXPOSE 9200 9300