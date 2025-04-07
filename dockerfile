# Base image
FROM --platform=linux/amd64 rocker/shiny:4.4.1

# Set the Python path for reticulate
ENV RETICULATE_PYTHON=/usr/bin/python3

# Set the working directory
WORKDIR /srv/shiny-server/app

# Install system dependencies
RUN apt-get update && \
    apt-get install -y python3 python3-pip libmagick++-dev && \
    pip3 install spacy && \
    python3 -m spacy download sv_core_news_md

# Install renv
RUN R -e "install.packages('renv')"

# Copy the app files and the renv.lock file
COPY app /srv/shiny-server/app
COPY renv.lock /srv/shiny-server/app/renv.lock

# Restore the R environment
RUN R -e "renv::restore()"

# Expose the Shiny Server port
EXPOSE 3838

# Run the Shiny server
CMD ["/usr/bin/shiny-server"]
