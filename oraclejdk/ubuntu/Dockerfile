FROM debian:stretch-slim
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

COPY jdk1.8 /app/jdk1.8
ENV JAVA_HOME=/app/jdk1.8 JRE_HOME=/app/jdk1.8/jre CLASSPATH=.:/app/jdk1.8/lib:/app/jdk1.8/lib PATH=.:/app/jdk1.8/bin:$PATH
#RUN mv /app/jdk1.8* /app/jdk1.8