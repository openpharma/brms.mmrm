ARG R_VERSION 

FROM rocker/r-ver:${R_VERSION}

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      cmake \
      libcurl4-openssl-dev \
      libgit2-dev \
      libglpk-dev \
      libglpk40 \
      libssl-dev \
      libxml2-dev \
      libzmq3-dev \
      xz-utils

RUN echo 'options(warn = 2L)' >> /root/.Rprofile

ARG GITHUB_PAT

COPY renv.lock renv.lock

RUN R -e 'install.packages("renv")'

RUN R -e 'renv::restore()'

CMD ["R", "--vanilla"]
