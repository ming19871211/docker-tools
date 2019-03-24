FROM ming19871211/oraclejdk:8-alpine
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

ARG MAVEN_VERSION=3.6.0
RUN apk --no-cache add tar && mkdir -p /app/maven \
    && wget http://mirrors.shu.edu.cn/apache/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz \
    && tar -xzf apache-maven-${MAVEN_VERSION}-bin.tar.gz -C /app/maven  --strip-components=1 \
    && rm -rf apache-maven-${MAVEN_VERSION}-bin.tar.gz
ENV MAVEN_HOME=/app/maven  PATH=/app/maven/bin:$PATH