FROM ming19871211/oraclejdk:8-arm64
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

ARG MAVEN_VERSION=3.6.1
RUN apt-get update && apt-get install -y wget tar && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /usr/share/maven \
    && wget http://mirror.bit.edu.cn/apache/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz \
    && tar -xzf apache-maven-${MAVEN_VERSION}-bin.tar.gz -C /usr/share/maven  --strip-components=1 \
    && rm -rf apache-maven-${MAVEN_VERSION}-bin.tar.gz
ENV MAVEN_HOME=/usr/share/maven  MAVEN_CONFIG="/root/.m2" PATH=/usr/share/maven/bin:$PATH