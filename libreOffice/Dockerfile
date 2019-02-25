FROM centos:7
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>
RUN yum -y install java-1.8.0-openjdk-devel libreoffice-*
ARG JAR_FILE
ARG PROFILE
ENV JAVA_OPTS="-server"
ENV PROFILE=${PROFILE}
ADD entrypoint.sh /data/
RUN chmod 777 /data/entrypoint.sh && ln -s /usr/lib64/libreoffice /usr/lib/libreoffice  \
    && ln -s /usr/lib64/libreoffice /opt/libreoffice5.4 \
    && mkdir -p /data02/service/convert/download/ \
    && mkdir -p /data02/service/convert/pdf/ \
    && mkdir -p /data02/service/convert/imgs/ 

ADD target/${JAR_FILE} /usr/share/myservice/myservice.jar
ENTRYPOINT bash /data/entrypoint.sh