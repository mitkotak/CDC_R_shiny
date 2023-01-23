FROM rocker/shiny:4.0.4
RUN install2.r rsconnect
WORKDIR /home/shinyusr
COPY shiny.R shiny.R 
COPY deploy.R deploy.R
CMD Rscript deploy.R
