#!/bin/bash
docker build -t ming19871211/skywalking-agent:6.6.0 .
docker build -t ming19871211/skywalking-agent:latest .
#测试
#docker run -d --name skywalking-agent ming19871211/skywalking-agent:6.6.0  tail -f /dev/null
#上传镜像
docker push ming19871211/skywalking-agent:6.6.0
docker push ming19871211/skywalking-agent:latest
