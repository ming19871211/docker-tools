#!/bin/bash
docker build -t ming19871211/yapi-cli:latest .
#测试
#docker run -d --name yapi-cli ming19871211/yapi-cli:latest tail -f /dev/null
#上传镜像
docker push ming19871211/yapi-cli:latest
