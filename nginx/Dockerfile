FROM debian:stretch-slim
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>
RUN apt-get update && apt-get -y install g++ openssl libssl-dev libpcre3 libpcre3-dev zlib1g-dev make git wget tar && mkdir /tmp-install
WORKDIR /tmp-install
ENV VERSION=1.14.2 NGINX_PATH=/etc/nginx
RUN wget http://nginx.org/download/nginx-${VERSION}.tar.gz && tar -xzvf nginx-${VERSION}.tar.gz \
    && mv nginx-${VERSION} nginx \
    && git clone https://github.com/alibaba/nginx-http-concat 
WORKDIR /tmp-install/nginx
RUN ./configure \
    --prefix=${NGINX_PATH} \
    --sbin-path=${NGINX_PATH}/nginx \
    --conf-path=${NGINX_PATH}/nginx.conf \
    --pid-path=${NGINX_PATH}/nginx.pid \
    --with-http_stub_status_module \
    --with-http_gzip_static_module \
    --with-http_ssl_module \
    --add-module=../nginx-http-concat \
    && make && make install \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /tmp-install \
    && ln -sf /dev/stdout ${NGINX_PATH}/logs/access.log \
	&& ln -sf /dev/stderr ${NGINX_PATH}/logs/error.log \
    && mkdir -p ${NGINX_PATH}/conf.d/ 
COPY nginx.conf ${NGINX_PATH}/nginx.conf
COPY default.conf ${NGINX_PATH}/conf.d/default.conf
WORKDIR ${NGINX_PATH}
EXPOSE 80
STOPSIGNAL SIGTERM
CMD  ["./nginx", "-g", "daemon off;"]
