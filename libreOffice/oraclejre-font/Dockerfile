FROM debian:11
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>
LABEL role='libreoffice' version='1.0.0' tags='libreoffice' description='libfreoffice conversion with jre8 support'
ENV TIMEZONE="Asia/Shanghai" VERSION=7.1.8 JAVA_HOME=/usr/local/jre1.8 PATH=/usr/local/jre1.8/bin:$PATH
# 复制中文字体
ADD cn-fonts.tar.gz /usr/share/fonts/
ADD jre-8*.tar.gz /usr/local
# 安装
RUN mv /usr/local/jre1.8* /usr/local/jre1.8 \
&& cp /etc/apt/sources.list /etc/apt/sources.list.bak \
&& sed -e 's/deb.debian.org/mirrors.aliyun.com/g' -i  /etc/apt/sources.list \
&& apt-get update \
&& apt-get -y install fontconfig wget procps libxinerama1 libdbus-1-3 libglib2.0-0 libcairo2 libcups2 libsm6 libx11-xcb1 \
&& fc-cache -fv && rm -rf /var/lib/apt/lists/* 
# 安装LibreOffice
RUN wget https://download.documentfoundation.org/libreoffice/stable/${VERSION}/deb/x86_64/LibreOffice_${VERSION}_Linux_x86-64_deb.tar.gz \
&& mkdir LibreOffice \
&& tar -zxvf LibreOffice_${VERSION}_Linux_x86-64_deb.tar.gz -C LibreOffice \
&& dpkg -i LibreOffice/LibreOffice_${VERSION}.*_Linux_x86-64_deb/DEBS/*.deb \
&& ln -s /opt/libreoffice7.1 /usr/lib/libreoffice \
&& mkdir /usr/lib64 && ln -s /opt/libreoffice7.1 /usr/lib64/libreoffice \
&& ln -s /opt/libreoffice7.1/program/soffice /usr/local/bin/libreoffice \
&& rm -rf LibreOffice*