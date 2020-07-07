FROM rocker/binder:3.6.3
LABEL maintainer='Flor'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/flor14/r_de_reproducibilidad/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2020-07-07/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
