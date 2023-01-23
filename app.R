#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
install.packages('shinythemes')
library(shinythemes)
library(reshape2)
library(plyr)
library(dplyr)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(theme= shinytheme("yeti"),
                navbarPage(
                  "CDC",
                  # Calculate Abbott formula
                  # correct mortality = ((mortality in test bottles[%] - mortality in control bottle[%]))*100)
                  # /(100% - mortality in control bottle[%])
                  tabsetPanel(
                    tabPanel("Abott Formula Calculator", fluid=TRUE,
                             sidebarLayout(
                               sidebarPanel(
                                 tags$h3("Input Percent Mortality:"),
                                 numericInput("MT", "Mortality in Test Bottle[%]:", value=NULL),
                                 numericInput("MC", "Mortality in Control Bottle[%]:", value=NULL),
                                 numericInput("Observation1", "Number of Observations", value=NULL),
                                 numericInput("Mortality1", "Total Mortality", value=NULL),
                               ), #sidebarPanel
                               mainPanel( textOutput("txtout")))),
                    tabPanel("Data Upload", "Data Input", fluid=TRUE,
                             sidebarLayout(
                               sidebarPanel(
                                 fileInput('file1', 'CSV File', accept= c('test/csv', 'text,comma-separated-values,text/plain', ' .csv')),
                                 radioButtons('sep', 'Separator', c(Comma=',',Semicolon=';', Tab='\t'),','),
                                 textInput("missing", "Missing Value Designator", value=NULL),
                                 checkboxInput('header', 'Header', TRUE),
                                 checkboxInput('show', 'Show all Rows', TRUE)),
                                 #numericInput("Variable1", "Mortality in Test Bottle[%]:", value=NULL),
                                 #numericInput("Variable2", "Mortality in Control Bottle[%]:", value=NULL),
                               mainPanel(tableOutput("contents"))),
                              position=c("left")),
                    
                    tabPanel("Statistics", fluid=TRUE,
                             sidebarLayout(
                               sidebarPanel(
                                 actionButton("stat", "Run Analysis"),
                                 downloadButton("down3", "Download Table")),
                               mainPanel(tableOutput("results"))),
                             position=c("left")),
                    
                    tabPanel("Density Plots", fluid=TRUE,
                             sidebarLayout(
                               sidebarPanel(
                                 uiOutput("checkbox2"),
                                 downloadButton("down1", "Download Plot")),
                               mainPanel(plotlyOutput("density")),
                             position=c("left")),
                    )
                ))
                    )

# Define server function
server <- function(input, output) {
  
  Abbott = reactive({
    MT = input$MT
    MC = input$MC
    equation = ((MT - MC) * 100)/(100 - MC)
    print(equation)
  })
  
      output$txtout = renderPrint({
        Abbott()
      })
  
  Mortality = reactive({
    req(input$file1)
    inFile = input$file1
    df = read.csv(inFile$datapath, header = input$header, sep = input$sep, na.strings = input$missing)
    M1 = input$Observation1
    M2 = input$Mortality1
    graph1 = ggplot(df, aes (x=M1), fill="blue", alpha=0.5) + geom_density() + theme_bw()
    graph2 = ggplot(df, aes (x=M2), fill="yellow", alpha=0.5) + geom_density() + theme_bw()
    return (graph1, graph2)
    
  })
  
  STATs = reactive({
    req(input$file1)
    inFile = input$file1
    df = read.csv(inFile$datapath, header = input$header, sep = input$sep, na.strings = input$missing)
    df2 = melt(df, measure.vars = colnames(df))
    df2$value = as.numeric(df2$value)
    df2$variable = as.character(df2$variable)
    df3 = na.omit(df2)
    df4 = ddply(df3 = variable, summarize, Minimum = min(value), Mean = mean(value), Median = quantile(value, 0.5), Maximum=max(value))
    return (df4)
  })
  
  output$results = renderTable({
    STATs()
  })
  
  Upload = reactive({
    req(input$file1)
    inFile = input$file1
    df = read.csv(inFile$datapath, header = input$header, sep = input$sep, na.strings = input$missing)
    return (df)
  })
  
  output$contents = renderTable({
    Upload()
  })
} #server

# Run the application 
shinyApp(ui = ui, server = server)
