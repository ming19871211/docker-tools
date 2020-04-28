FROM r-base:3.3.1
MAINTAINER Meiqiming  <Meiqiming@talkweb.com.cn>
#apt源参考地址 https://packages.debian.org/stretch/amd64/libmysql++-dev/download
RUN mv /etc/apt/sources.list /etc/apt/sources.list.bak \
    && mv /etc/apt/sources.list.d/debian-unstable.list /etc/apt/sources.list.d/debian-unstable.list.bak \
    && mv /etc/apt/apt.conf.d/default /etc/apt/apt.conf.d/default.bak \
    && echo 'APT::Default-Release "stretch";' > /etc/apt/apt.conf.d/default \
    && echo "deb http://ftp.cn.debian.org/debian stretch main" > /etc/apt/sources.list \
    && echo "deb http://ftp.cn.debian.org/debian stretch-updates main" >> /etc/apt/sources.list \
    && echo "deb http://ftp.cn.debian.org/debian-security stretch/updates main" >> /etc/apt/sources.list \
    && apt-get update \
    && apt-get -y install wget tar gcc g++ libmysql++-dev \
    && rm -rf /var/lib/apt/lists/*

ENV R_SOURCE_PATH=/R/rscript/ CONTAINER_PORT=6311
COPY ./rscript/  ${R_SOURCE_PATH}

RUN R -e "source(file='${R_SOURCE_PATH}/package_install.R')" \
    && R -e "source(file='${R_SOURCE_PATH}/load_source.R')" \
    && R -e "library('abind')" \
    && R -e "library('magrittr')" \
    && R -e "library('plyr')" \
    && R -e "library('Rcpp')" \
    && R -e "library('reshape2')" \
    && R -e "library('Rserve')" \
    && R -e "library('stringi')" \
    && R -e "library('stringr')" \
    && R -e "library('DBI')" \
    && R -e "library('splitstackshape')" \
    && R -e "library('data.table')" \
    && R -e "library('RMySQL')" \
    && R -e "library('tmcn')" 

ENTRYPOINT ["bash","-c","R CMD Rserve --RS-enable-remote --RS-port ${CONTAINER_PORT} --vanilla && tail -f /dev/null"]
