FROM ming19871211/oraclejdk:8-alpine
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

ARG MAVEN_VERSION=3.6.0
RUN apk --no-cache add tar && mkdir -p /usr/share/maven \
    && wget http://mirrors.shu.edu.cn/apache/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz \
    && tar -xzf apache-maven-${MAVEN_VERSION}-bin.tar.gz -C /usr/share/maven  --strip-components=1 \
    && rm -rf apache-maven-${MAVEN_VERSION}-bin.tar.gz
ENV MAVEN_HOME=/usr/share/maven  MAVEN_CONFIG="/root/.m2" PATH=/usr/share/maven/bin:$PATH