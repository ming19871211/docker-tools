FROM node:12-alpine
MAINTAINER Meiqiming  <ming19871211@163.com>

ENV TZ="Asia/Shanghai"  HOME="/data"

RUN mkdir -p /data \
    && npm config set registry https://registry.npm.taobao.org \
    && npm install -g yapi-cli

WORKDIR ${HOME}

CMD ["yapi" "import"]