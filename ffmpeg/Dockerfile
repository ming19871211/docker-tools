FROM openjdk:8-jre
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

RUN apt-get update && apt-get -y install g++ make  yasm wget tar && mkdir /tmp-install
WORKDIR /tmp-install
ENV VERSION=4.0.3 
RUN wget http://ffmpeg.org/releases/ffmpeg-${VERSION}.tar.gz && tar -xzvf ffmpeg-${VERSION}.tar.gz \
    && mv ffmpeg-${VERSION} ffmpeg  && rm -rf /var/lib/apt/lists/*
WORKDIR /tmp-install/ffmpeg
RUN apt-get install yasm && ./configure && make && make install \
    && rm -rf /tmp-install/ffmpeg/* && rm -f /tmp-install/ffmpeg-${VERSION}.tar.gz
