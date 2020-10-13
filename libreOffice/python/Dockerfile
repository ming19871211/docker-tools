FROM python:3.7.4
MAINTAINER QiMing Mei <meiqiming@talkweb.com.cn>
RUN apt -y update && apt install -y libreoffice libreoffice-l10n-zh-cn; \
    rm -rf /var/lib/apt/lists/*;  \
    pip install torch==1.3.1+cpu torchvision==0.4.2+cpu -f https://download.pytorch.org/whl/torch_stable.html;