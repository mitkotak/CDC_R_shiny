FROM rocker/shiny:latest
RUN install2.r rsconnect
RUN install2.r shinythemes
WORKDIR /home/shinyusr
COPY app.R app.R 
COPY deploy.R deploy.R
CMD Rscript deploy.R
