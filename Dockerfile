FROM rocker/shiny-verse:4.3.2

LABEL description="This is the docker container for boxes"

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
	RMySQL \
	remotes \
	renv

RUN R -e 'remotes::install_github("Appsilon/rhino")'

WORKDIR /srv/shiny-server

COPY . . 

RUN R -e 'renv::deactivate()'

