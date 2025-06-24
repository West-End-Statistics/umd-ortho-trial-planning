FROM westendstatistics/rstats-dev:r4.5.1-20250623 AS development

USER root
RUN mkdir /opt/cmdstan
RUN Rscript -e "cmdstanr::install_cmdstan(dir = '/opt/cmdstan', cores = 2)"  
ENV CMDSTAN="/opt/cmdstan"
RUN install2.r --error \
  BuyseTest

USER vscode