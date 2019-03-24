FROM alpine:3.9
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>
#glib安装参考网址 https://github.com/sgerrand/alpine-pkg-glibc

ADD jdk-8*.tar.gz /app/
ENV JAVA_HOME=/app/jdk1.8 JRE_HOME=/app/jdk1.8/jre CLASSPATH=.:/app/jdk1.8/lib:/app/jdk1.8/lib PATH=.:/app/jdk1.8/bin:$PATH
RUN mv /app/jdk1.8* /app/jdk1.8 \
    && echo https://mirrors.aliyun.com/alpine/v3.9/main > /etc/apk/repositories  \
    && echo https://mirrors.aliyun.com/alpine/v3.9/community >> /etc/apk/repositories \
    && apk update && apk upgrade \
    && apk --no-cache add ca-certificates wget \
    && wget -q -O /etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub \
    && wget https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.29-r0/glibc-2.29-r0.apk \
    && apk add glibc-2.29-r0.apk



