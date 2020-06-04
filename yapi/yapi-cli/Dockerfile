FROM node:12-alpine
MAINTAINER Meiqiming  <ming19871211@163.com>

ENV TZ="Asia/Shanghai"  HOME="/data"

COPY docker-entrypoint.sh /

RUN mkdir -p /data && chmod 777 /docker-entrypoint.sh \
    && npm config set registry https://registry.npm.taobao.org \
    && npm install -g yapi-cli 

WORKDIR ${HOME}

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["yapi","import"]
