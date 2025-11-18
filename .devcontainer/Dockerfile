FROM westendstatistics/rstats-dev:r4.5.1-20251111 AS development

USER root

# Install system dependencies
RUN apt-get update && apt-get install -y libwebp-dev && rm -rf /var/lib/apt/lists/*

# RUN mkdir /opt/cmdstan
# RUN Rscript -e "cmdstanr::install_cmdstan(dir = '/opt/cmdstan', cores = 2)"
# ENV CMDSTAN="/opt/cmdstan"
# RUN install2.r --error \
#   BuyseTest ordinal shiny truncnorm WinRatio

# # Install Node.js 18+ and npm
# RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
#   && apt-get install -y nodejs

# # Install Claude Code globally
# RUN npm install -g @anthropic-ai/claude-code

# # Ensure vscode user can access claude
# RUN chown -R vscode:vscode /usr/lib/node_modules/@anthropic-ai/claude-code || true
# RUN chmod +x /usr/bin/claude || true

RUN install2.r --error \
  devtools
USER vscode

RUN Rscript -e "dir.create(path = Sys.getenv('R_LIBS_USER'), showWarnings = FALSE, recursive = TRUE)"
# RUN Rscript -e "dir.create(path = Sys.getenv('R_LIBS_USER'), showWarnings = FALSE, recursive = TRUE)"
RUN   echo ".libPaths(Sys.getenv('R_LIBS_USER'))" >> /home/vscode/.Rprofile


# # This always invalides the cache so the 
# RUN echo $(od -vAn -N2 -tu2 < /dev/urandom)
# RUN echo $(ls -l /workspaces)
# RUN Rscript -e "devtools::install('/workspaces/umd-ortho-trial-planning/', quick = TRUE, upgrade = 'never', build_vignettes = FALSE)"
