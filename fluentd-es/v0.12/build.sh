#!/bin/bash
docker build --force-rm --no-cache -t  ming19871211/fluentd-es:v0.12 .
docker push ming19871211/fluentd-es:v0.12 