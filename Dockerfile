FROM adoptopenjdk/openjdk11:latest
RUN apt-get update && apt-get install -y \
    lilypond
MAINTAINER github.balath
RUN mkdir output
RUN mkdir models
RUN mkdir data
COPY /data/rawData.txt /data/rawData.txt
COPY bach-machine.jar bach-machine.jar
COPY /models/minor.model /models/minor.model
COPY /models/major.model /models/major.model
COPY daemon.sh daemon.sh
COPY start.sh start.sh
RUN chmod +x daemon.sh
RUN chmod +x start.sh
ENTRYPOINT ./start.sh
EXPOSE 8080