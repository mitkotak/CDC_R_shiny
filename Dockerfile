FROM rocker/shiny:latest
RUN install2.r shinyscreenshot
RUN install2.r samplingbook
RUN install2.r tidyverse
RUN install2.r shinyMatrix
RUN install2.r rsconnect
RUN install2.r shinythemes
RUN install2.r reshape2
RUN install2.r plyr
RUN install2.r dplyr
RUN install2.r plotly
RUN install2.r tibble
RUN install2.r DT
WORKDIR /home/shinyusr
COPY app.R app.R 
COPY deploy.R deploy.R
CMD Rscript deploy.R
