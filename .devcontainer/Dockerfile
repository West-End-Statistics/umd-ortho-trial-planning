FROM rocker/r-ver:4.4 as base
RUN /rocker_scripts/setup_R.sh \
    https://packagemanager.posit.co/cran/__linux__/jammy/2024-05-01
    # https://code.visualstudio.com/docs/devcontainers/create-dev-container#:~:text=Note%3A%20The%20DEBIAN_FRONTEND%20export%20avoids%20warnings%20when%20you%20go%20on%20to%20work%20with%20your%20container.
RUN apt-get update --fix-missing && export DEBIAN_FRONTEND=noninteractive
# https://notes.rmhogervorst.nl/post/2020/09/23/solving-libxt-so-6-cannot-open-shared-object-in-grdevices-grsoftversion/
RUN apt-get install -y --no-install-recommends libxt6
# Install Latest Quarto
RUN apt-get install -y --no-install-recommends \
    gdebi-core pandoc curl \
    # for igraph 
    libglpk-dev \
    && rm -rf /var/lib/apt/lists/*
RUN curl -LO https://quarto.org/download/latest/quarto-linux-amd64.deb
RUN gdebi --non-interactive quarto-linux-amd64.deb

# Install R packages
RUN install2.r --error \
    bayesDP \
    bayesplot \
    broom \
    broom.mixed \
    brms \
    checkmate \
    collapse \
    crew \
    dplyr \
    emmeans \
    flextable \
    ggplot2 \
    gsDesign \
    gtsummary \
    here \
    knitr \
    lme4 \
    lmerTest \
    memoise \
    microbenchmark \
    mvtnorm \
    mmrm \
    pander \
    plotly \
    purrr \
    quarto \
    readr \
    readxl \
    renv \
    rmarkdown \
    rstan \
    tarchetypes \
    targets\
    tibble \
    tidyr \
    tree \
    && rm -rf /tmp/downloaded_packages \
    && strip /usr/local/lib/R/site-library/*/libs/*.so
RUN Rscript -e 'install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos"))); cmdstanr::install_cmdstan(cores = 2)'  
FROM base as development
# https://www.makeuseof.com/install-python-ubuntu/
# install radian and python
RUN apt-get update && apt-get -y install software-properties-common
RUN add-apt-repository -y ppa:deadsnakes/ppa && apt-get update
RUN apt-get -y install python3.11 python3-pip git
RUN pip3 install radian
# end install radian

# Packages needed for development with vscode
RUN install2.r --error \
    languageserver jsonlite rlang httpgd \
    && rm -rf /tmp/downloaded_packages \
    && strip /usr/local/lib/R/site-library/*/libs/*.so

# Other packages to install - move away from dev when ready
# RUN install2.r --error \
#     bayesDP furrr memoise plotly