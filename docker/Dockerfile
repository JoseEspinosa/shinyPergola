############################################################
# Dockerfile to build shinnyPergola
# Based on Ubuntu
############################################################

# Set the base image to Ubuntu
FROM ubuntu

# FROM rocker/r-base

MAINTAINER Jose Espinosa-Carrasco <espinosacarrascoj@gmail.com>

#
# Install Python and Basic Python Tools
#

RUN apt-get install -y python python-dev python-distribute python-pip

#
# Install Pergola
#
 
RUN wget -q https://github.com/cbcrg/pergola/archive/master.zip && \
    unzip master.zip && \
    cd pergola-master && \
    python setup.py install 
#
# Get pip to download and install requirements:
#

# TODO add requirements.txt file to pergola
RUN pip install -r /pegola-master/requirements.txt
	