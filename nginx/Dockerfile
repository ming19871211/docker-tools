FROM debian:stretch-slim
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>
RUN apt-get update && apt-get -y install g++ openssl libssl-dev libpcre3 libpcre3-dev zlib1g-dev make git wget tar && mkdir /tmp-install
WORKDIR /tmp-install
RUN wget http://nginx.org/download/nginx-1.14.2.tar.gz && tar -xzvf nginx-1.14.2.tar.gz \
    && mv nginx-1.14.2 nginx \
    && git clone https://github.com/alibaba/nginx-http-concat 
WORKDIR /tmp-install/nginx
RUN ./configure \
    --sbin-path=/usr/local/nginx/nginx \
    --conf-path=/usr/local/nginx/nginx.conf \
    --pid-path=/usr/local/nginx/nginx.pid \
    --with-http_stub_status_module \
    --with-http_gzip_static_module \
    --with-http_ssl_module \
    --add-module=../nginx-http-concat \
    && make && make install \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /tmp-install \
    && ln -sf /dev/stdout /usr/local/nginx/logs/access.log \
	&& ln -sf /dev/stderr /usr/local/nginx/logs/error.log \
    && mkdir -p /etc/nginx/conf.d/ 
COPY nginx.conf /usr/local/nginx/nginx.conf
COPY default.conf /etc/nginx/conf.d/default.conf
WORKDIR /usr/local/nginx/
EXPOSE 80
STOPSIGNAL SIGTERM
CMD  ["/usr/local/nginx/nginx", "-g", "daemon off;"]
