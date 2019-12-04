FROM openjdk:8-jre
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

RUN apt-get update \
    && apt-get -y install build-essential \
    && apt-get -y install cmake libgtk2.0-dev pkg-config libavcodec-dev libavformat-dev libswscale-dev \
    && apt-get -y install python-dev python-numpy libtbb2 libtbb-dev libjpeg-dev libpng-dev libtiff-dev  libdc1394-22-dev \
    && apt-get -y install wget tar g++\
    && mkdir /tmp-install
WORKDIR /tmp-install
ENV VERSION=4.1.1 
RUN wget https://github.com/opencv/opencv/archive/${VERSION}.tar.gz && tar -xzvf ${VERSION}.tar.gz \
    && mv opencv-${VERSION} opencv  && mkdir opencv/build && rm -rf /var/lib/apt/lists/*
WORKDIR /tmp-install/opencv/build
RUN cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local .. \
    && make install