FROM openjdk:8-jdk
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>

ENV VERSION=4.1.1
RUN apt-get update \
    && apt-get -y install build-essential \
    && apt-get -y install cmake libgtk2.0-dev pkg-config libavcodec-dev libavformat-dev libswscale-dev \
    && apt-get -y install python-dev python-numpy libtbb2 libtbb-dev libjpeg-dev libpng-dev libtiff-dev libdc1394-22-dev \
    && apt-get -y install wget tar g++ ant \
    && rm -rf /var/lib/apt/lists/*
RUN mkdir /tmp-install && cd /tmp-install && wget https://github.com/opencv/opencv/archive/${VERSION}.tar.gz \
    && tar -xzvf ${VERSION}.tar.gz && rm -rf ${VERSION}.tar.gz \
    && mv opencv-${VERSION} opencv && mkdir opencv/build && cd /tmp-install/opencv/build \
    && cmake -D CMAKE_BUILD_TYPE=RELEASE -D BUILD_TESTS=OFF -D CMAKE_INSTALL_PREFIX=/usr/local .. \
    && make && make install \
    && cp /tmp-install/opencv/build/lib/libopencv_java* /usr/local/lib \
    && cd ~ && rm -rf /tmp-install \
    && echo "/usr/local/lib" >> /etc/ld.so.conf.d/opencv.conf && ldconfig -v 