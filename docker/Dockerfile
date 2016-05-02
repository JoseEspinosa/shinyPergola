############################################################
# Dockerfile to build shinnyPergola
# Based on Ubuntu
############################################################

# Set the base image to Ubuntu
# wheezy is small only 30 mb
FROM debian:wheezy

# It already has the OS, then comment wheezy
# FROM rocker/r-base

MAINTAINER Jose Espinosa-Carrasco <espinosacarrascoj@gmail.com>

#
# Install Python and Basic Python Tools
#
# Update always before download
# single command save space, becuase each run generates a folder layer
RUN apt-get update && \
apt-get install -y python python-dev python-distribute python-pip

# All version before talking with Paolo, he suggest that is better not to use the get
# the repo from github if we are going to create the image from our local computer
#
# Install Pergola
#
 
# RUN wget -q https://github.com/cbcrg/pergola/archive/master.zip && \
#    unzip master.zip && \
#    cd pergola-master && \
#    python setup.py install 
#
# Get pip to download and install requirements:
#

# TODO add requirements.txt file to pergola
#RUN pip install -r /pegola-master/requirements.txt
	














