ARG R_VERSION 
FROM rocker/rstudio:${R_VERSION}
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      cmake \
      libcurl4-openssl-dev \
      libgit2-dev \
      libglpk-dev \
      libglpk40 \
      libssl-dev \
      libxml2-dev \
      xz-utils
RUN echo 'options(warn = 2L)' >> /root/.Rprofile
RUN Rscript -e 'install.packages("renv")'
COPY ./renv.lock ./renv.lock
ARG GITHUB_PAT
RUN Rscript -e 'renv::restore()'
CMD ["R", "--vanilla"]
