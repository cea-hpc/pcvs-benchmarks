FROM python:3

WORKDIR /pcvs

RUN apt update && apt install -y git tar gcc g++ gfortran cron
RUN git clone --depth 1 --branch devel https://github.com/cea-hpc/pcvs.git /pcvs
RUN pip install --no-cache-dir -r /pcvs/requirements.txt
RUN pip install /pcvs/.

RUN git clone --depth 1 --branch devel https://github.com/cea-hpc/pcvs-benchmarks.git /benchmark
RUN git clone --depth 1 --branch develop https://github.com/spack/spack.git /spack
ENV PATH="${PATH}:/spack/bin"
COPY run.sh /script/
COPY cronjob.txt /script/
COPY profiles/ /profiles/

RUN pcvs profile import user.mpi -s /profiles/mpi.yml
RUN pcvs profile import user.mpc-mpi -s /profiles/mpc-mpi.yml
RUN pcvs profile import user.intelmpi -s /profiles/intelmpi.yml

RUN pcvs bank init mpicheck /bank
ENV MPIMETA /benchmark/MPI/mpicheck
RUN crontab /script/cronjob.txt
RUN touch /var/log/cron.log
CMD cron && tail -f /var/log/cron.log
