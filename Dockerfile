FROM debian:jessie
MAINTAINER Ivan Valle <ivanmartinvalle@gmail.com>

RUN apt-get update && apt-get install -y \
    build-essential \
    gnat \
    gprbuild \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

ADD . /root/.whitakers-words

WORKDIR /root/.whitakers-words

RUN make

ENTRYPOINT ["./bin/words"]
