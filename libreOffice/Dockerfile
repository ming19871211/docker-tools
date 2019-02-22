FROM openjdk:8-jre
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

RUN apt-get update && apt-get -y install libreoffice ghostscript

# ARG JAR_FILE
# ARG PROFILE
# ENV JAVA_OPTS="-server"
# ENV PROFILE=${PROFILE}
# ENTRYPOINT /usr/bin/java ${JAVA_OPTS} -jar /usr/share/myservice/myservice.jar --spring.profiles.active=${PROFILE}

# ADD target/${JAR_FILE} /usr/share/myservice/myservice.jar