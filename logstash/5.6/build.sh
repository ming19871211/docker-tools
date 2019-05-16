#!/bin/bash
docker build -t ming19871211/logstash:5.6 .


# RUN rm -f /usr/share/logstash/pipeline/logstash.conf
# ADD pipeline/ /usr/share/logstash/pipeline/
# ADD config/ /usr/share/logstash/config/