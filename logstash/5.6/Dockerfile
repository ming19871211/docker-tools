FROM docker.elastic.co/logstash/logstash:5.6.16
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>
# wget https://artifacts.elastic.co/downloads/logstash/logstash-5.6.16.tar.gz

RUN mkdir plugins \
&& curl -L -# -o plugins/logstash-filter-jdbc_streaming.tar.gz  https://github.com/logstash-plugins/logstash-filter-jdbc_streaming/archive/v1.0.6.tar.gz \
&& tar -xzf plugins/logstash-filter-jdbc_streaming.tar.gz -C plugins/ \
&& curl -L -# -o plugins/logstash-input-jdbc.tar.gz https://github.com/logstash-plugins/logstash-input-jdbc/archive/v4.3.13.tar.gz \
&& tar -xzf plugins/logstash-input-jdbc.tar.gz -C plugins/ \
&& rm -f plugins/logstash-filter-jdbc_streaming.tar.gz plugins/logstash-input-jdbc.tar.gz \
&& sed -i  -e "s/@scheduler.cron @schedule do/@scheduler.every @schedule do/g"  plugins/logstash-input-jdbc-4.3.13/lib/logstash/inputs/jdbc.rb \
&& sed -i  -e "s/process_event(event, @default_array)/process_event(event, [])/g"  plugins/logstash-filter-jdbc_streaming-1.0.6/lib/logstash/filters/jdbc_streaming.rb \
&& sed -i  -e 's/gem "logstash-input-jdbc"/gem "logstash-input-jdbc", "4.3.13", :path => "plugins\/logstash-input-jdbc-4.3.13"/g'  Gemfile \
&& echo 'gem "logstash-filter-jdbc_streaming", "1.0.6", :path => "plugins/logstash-filter-jdbc_streaming-1.0.6"' >>Gemfile \
&& bin/logstash-plugin install --no-verify