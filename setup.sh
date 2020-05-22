#!/bin/bash

apt-get update -y
apt install software-properties-common -y
add-apt-repository ppa:plt/racket -y
apt-get install -y racket
apt-get clean
