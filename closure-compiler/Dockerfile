FROM openjdk:8-jre-alpine
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

ENV VERSION=latest COMPILER_JAR=/opt/compiler.jar
RUN wget -P /tmp https://dl.google.com/closure-compiler/compiler-${VERSION}.tar.gz \
&& tar -xzvf /tmp/compiler-latest.tar.gz -C /tmp \
&& mv /tmp/closure-compiler-*.jar  /opt/compiler.jar \
&& rm /tmp/* 

#ENTRYPOINT ["java -jar $COMPILER_JAR --js_output_file=out.js '**.js'"]