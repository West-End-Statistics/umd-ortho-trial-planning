FROM westendstatistics/rstats-dev:r4.5.1-20250701 AS development

USER root
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

USER vscode
