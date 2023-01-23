FROM rocker/shiny:latest
RUN install2.r rsconnect
RUN install2.r shinythemes
RUN install2.r reshape2
RUN install2.r plyr
RUN install2.r dplyr
RUN install2.r plotly
WORKDIR /home/shinyusr
COPY app.R app.R 
COPY deploy.R deploy.R
CMD Rscript deploy.R
