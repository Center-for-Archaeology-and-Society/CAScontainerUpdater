FROM rocker/shiny-verse:4.3.1

LABEL description="This is the docker container for Archaeodash"

RUN apt-get update

RUN  apt-get install -y --no-install-recommends \
	vim \
	git \
	libcurl4-openssl-dev \
	libssl-dev \
	libxml2-dev \
	libcairo2-dev \
	libxt-dev \
	libmysqlclient-dev \
	libssh-dev

RUN install2.r --error --skipinstalled \
	dplyr \
	DT \
	magrittr \
	rio \
	shiny \
	shinyjs \
	janitor \
	devtools \
	tidyr \
	stringr \
	shinyauthr \
	readr \
	ssh \
	DBI \
	dbplyr \
	RMySQL
