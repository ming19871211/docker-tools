#!/bin/bash
docker build -t ming19871211/node:8-cnpm .
#测试
#docker run -d --name node-cnpm ming19871211/node:8-cnpm tail -f /dev/null
#上传镜像
docker push ming19871211/node:8-cnpm