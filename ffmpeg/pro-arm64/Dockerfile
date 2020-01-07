FROM arm64v8/openjdk:8-jre
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

RUN apt-get update && apt-get -y install g++ make  yasm wget tar  && mkdir /tmp-install
WORKDIR /tmp-install
ENV VERSION=4.2.1
RUN wget http://ffmpeg.org/releases/ffmpeg-${VERSION}.tar.bz2 && tar -xf ffmpeg-${VERSION}.tar.bz2  && wget http://sourceforge.net/projects/lame/files/lame/3.99/lame-3.99.5.tar.gz   && tar -xf lame-3.99.5.tar.gz \
    &&  wget http://downloads.xvid.org/downloads/xvidcore-1.1.0.tar.gz  && tar -xf  xvidcore-1.1.0.tar.gz  &&  wget http://sourceforge.net/projects/opencore-amr/files/opencore-amr/opencore-amr-0.1.3.tar.gz  && tar -xf opencore-amr-0.1.3.tar.gz \
    && mv ffmpeg-${VERSION} ffmpeg  && rm -rf /var/lib/apt/lists/*
WORKDIR /tmp-install/ffmpeg
RUN cd /tmp-install/lame-3.99.5/  &&  ./configure --enable-shared --build=arm-linux  && make && make install \
    && cd /tmp-install/xvidcore-1.1.0/build/generic  && ./configure --build=arm-linux && make  && make install \
    && cd /tmp-install/opencore-amr-0.1.3   && ./configure --build=arm-linux && make && make install   \
    && cd /tmp-install/ffmpeg/  &&   ./configure --enable-libopencore-amrnb --enable-shared --enable-libmp3lame --enable-libopencore-amrwb --enable-gpl --enable-version3 --build=arm-linux && make && make install \
    && cp -r /usr/local/lib/* /usr/lib/    && rm -rf /tmp-install/*  && mkdir /tmp-install/ffmpeg  
