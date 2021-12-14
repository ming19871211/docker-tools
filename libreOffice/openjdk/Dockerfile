FROM openjdk:8-jdk
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>
LABEL role='libreoffice' version='1.0.0' tags='libreoffice' description='libfreoffice conversion with jre8 support'
ENV TIMEZONE="Asia/Shanghai" VERSION=7.1.8

# 安装中文字体
RUN apt-get -y install locales-all fonts-wqy-microhei fonts-wqy-zenhei fontconfig    libxt6 libxrender1

RUN apt-get -y install libxinerama1 libdbus-1-3 libglib2.0-0 libcairo2 libcups2 libsm6 libx11-xcb1

RUN wget https://download.documentfoundation.org/libreoffice/stable/7.1.8/deb/x86_64/LibreOffice_7.1.8_Linux_x86-64_deb.tar.gz
RUN wget https://download.documentfoundation.org/libreoffice/stable/7.1.8/deb/x86_64/LibreOffice_7.1.8_Linux_x86-64_deb_langpack_zh-CN.tar.gz
RUN tar -zxvf LibreOffice_7.1.8_Linux_x86-64_deb.tar.gz
RUN tar -zxvf LibreOffice_7.1.8_Linux_x86-64_deb_langpack_zh-CN.tar.gzanz
# 安装
RUN dpkg -i ./LibreOffice_7.1.8.*_Linux_x86-64_deb/DEBS/*.deb
# 安装中文包
RUN dpkg -i ./LibreOffice_7.1.8.*_Linux_x86-64_deb_langpack_zh-CN/DEBS/*.deb
# 增加字体
RUN wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-serif/SubsetOTF/CN/SourceHanSerifCN-Bold.otf \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-serif/SubsetOTF/CN/SourceHanSerifCN-ExtraLight.otf \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-serif/SubsetOTF/CN/SourceHanSerifCN-Heavy.otf \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-serif/SubsetOTF/CN/SourceHanSerifCN-Light.otf \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-serif/SubsetOTF/CN/SourceHanSerifCN-Medium.otf \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-serif/SubsetOTF/CN/SourceHanSerifCN-Regular.otf \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-serif/SubsetOTF/CN/SourceHanSerifCN-SemiBold.otf \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-sans/SubsetOTF/CN/SourceHanSansCN-Bold.otf \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-sans/SubsetOTF/CN/SourceHanSansCN-ExtraLight.otf \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-sans/SubsetOTF/CN/SourceHanSansCN-Heavy.otf \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-sans/SubsetOTF/CN/SourceHanSansCN-Light.otf  \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-sans/SubsetOTF/CN/SourceHanSansCN-Medium.otf \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-sans/SubsetOTF/CN/SourceHanSansCN-Normal.otf  \
&& wget https://mirrors.tuna.tsinghua.edu.cn/adobe-fonts/source-han-sans/SubsetOTF/CN/SourceHanSansCN-Regular.otf 
# 刷新字体
RUN mkdir /usr/share/fonts/han &&  mv *.otf /usr/share/fonts/han &&  fc-cache -fv

# https://github.com/renothing/docker-libreoffice/blob/master/Dockerfile

# https://zh-cn.libreoffice.org/download/libreoffice/

# RUN sudo apt-get remove --purge libreoffice7.1-*  
# https://www.debian.org/

# https://www.cnblogs.com/iwetuan/p/13489736.html  ttf-wqy-zenhei

# https://www.bilibili.com/read/cv4827843/  
# https://www.docx4java.org/downloads.html  https://blog.csdn.net/weixin_34295316/article/details/86022702
# https://gitee.com/mirrors/docx4j