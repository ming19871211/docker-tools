FROM centos:7
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>
RUN yum -y install java-1.8.0-openjdk-devel libreoffice-* ghostscript && yum -y groupinstall 'Fonts' \
    && ln -s /usr/lib64/libreoffice /usr/lib/libreoffice