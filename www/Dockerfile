FROM ubuntu:14.04

RUN apt-get -y install openjdk-7-jre
RUN apt-get -y install wget

RUN apt-get remove scala-library scala

RUN wget http://downloads.typesafe.com/scala/2.11.1/scala-2.11.1.deb
RUN dpkg -i scala-2.11.1.deb
RUN apt-get update
RUN apt-get install scala
 
RUN wget http://dl.bintray.com/sbt/debian/sbt-0.13.5.deb
RUN dpkg -i sbt-0.13.5.deb
RUN apt-get update
RUN apt-get install sbt

ADD . /apidoc

WORKDIR /apidoc
RUN SBT_OPTS="-Xmx1G" sbt sbt-version
RUN export SBT_OPTS="-Xmx1G" && sbt www/stage

