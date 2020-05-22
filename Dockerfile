FROM gradescope/auto-builds:latest

RUN mkdir /autograder/source
RUN apt-get update -y
RUN apt install software-properties-common -y
RUN add-apt-repository ppa:plt/racket -y
RUN apt-get install -y racket
RUN apt-get clean
COPY autograder/run_autograder /autograder
COPY autograder/test.rkt /autograder/source/test.rkt

