############################################################
# Dockerfile to build shinnyPergola
# Based on Ubuntu
############################################################

# Set the base image to Ubuntu
FROM ubuntu

# FROM rocker/r-base

MAINTAINER Jose Espinosa-Carrasco <espinosacarrascoj@gmail.com>

#
# Install Pergola
# 
RUN wget -q https://github.com/cbcrg/pergola/archive/master.zip && \
    unzip master.zip && \
    cd pergola-master && \
    python setup.py install 

	