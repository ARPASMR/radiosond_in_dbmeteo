FROM arpasmr/r-base
RUN apt-get update
RUN apt-get install -y s3cmd
COPY . /usr/local/src/myscripts
WORKDIR /usr/local/src/myscripts
RUN chmod a+x *.sh
CMD ["./launcher.sh"]
