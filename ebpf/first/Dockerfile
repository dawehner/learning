FROM phusion/baseimage:0.9.18

RUN echo "deb [trusted=yes] https://repo.iovisor.org/apt/xenial xenial-nightly main" | sudo tee /etc/apt/sources.list.d/iovisor.list
RUN apt-get update && apt-get install bcc-tools -y && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir /usr/src/app
COPY *.py /usr/src/app

CMD ["sudo", "python", "/usr/src/app/hello_world.py"]
