#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(reshape2)
library(plyr)
library(dplyr)
library(plotly)
library("shinyMatrix")
library(DT)
library(tibble)


###function for deleting the rows
splitColumn <- function(data, column_name) {
  newColNames <- c("Unmerged_type1", "Unmerged_type2")
  newCols <- colsplit(data[[column_name]], " ", newColNames)
  after_merge <- cbind(data, newCols)
  after_merge[[column_name]] <- NULL
  after_merge
}

###_______________________________________________
### function for inserting a new column

fillvalues <- function(data, values, columName){
  df_fill <- data
  vec <- strsplit(values, ",")[[1]]
  df_fill <- tibble::add_column(df_fill, newcolumn = vec, .after = columName)
  df_fill
}

##function for removing the column

removecolumn <- function(df, nameofthecolumn){
  df[ , -which(names(df) %in% nameofthecolumn)]
}


m <- matrix(runif(6), 11, 6, dimnames = list(NULL, c("Time",
                                                       "Bottle 1M Dead",
                                                       "Bottle 2M Dead",
                                                       "Bottle 3P Dead",
                                                       "Bottle 4P Dead",
                                                       "Control Dead")))

m[,1] = c(0,5,10,15,30,45,60,75,90,105,120)

# Define UI for application that draws a histogram
ui <- fluidPage(theme= shinytheme("yeti"),
                navbarPage(
                  "CDC",
                  # Calculate Abbott formula
                  # correct mortality = ((mortality in test bottles[%] - mortality in control bottle[%]))*100)
                  # /(100% - mortality in control bottle[%])
                  tabsetPanel(
                    tabPanel("Insecticide Diagnostic Times", fluid=TRUE,
                             sidebarLayout(
                               sidebarPanel(
                                 fileInput("file1", "Choose CSV File", accept = ".csv"),
                                 checkboxInput("header", "Header", TRUE),
                                 actionButton("Splitcolumn", "SplitColumn"),
                                 uiOutput("selectUI"),
                                 actionButton("deleteRows", "Delete Rows"),
                                 textInput("textbox", label="Input the value to replace:"),
                                 actionButton("replacevalues", label = 'Replace values'),
                                 actionButton("removecolumn", "Remove Column"),
                                 actionButton("Undo", 'Undo')
                               ),
                               mainPanel(
                                 DTOutput("table1")))),
                    tabPanel("Abott Formula Calculator", fluid=TRUE,
                             sidebarLayout(
                               sidebarPanel(
                                 width=6,
                                 tags$h3("Data"),
                                 matrixInput("sample",
                                             value=m,
                                             rows=list(extend= TRUE),
                                             cols=list(names=TRUE),
                                             class='numeric')
                               ),
                               sidebarPanel(plotOutput("plot")))),
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
                             position=c("left"))),
                ))
                    )

# Define server function
server <- function(session, input, output) {
  library(tidyverse)
  insecticide_data <- read_table("Type  Conc.  Ae.aegypti  Ae.albopictus Cx.pipiens Cx.quinquefasciatus  Cx.tarsalis
  Chlorpyrifos  20  45  45  90  45  60
  Deltamethrin  0.75  30  30  45  60  -
  Etofenprox  12.5  15  30  15  30  60
  Fenthion  800  -  -  75  45  45  45
  Malathion  400  15  30  45  45  45
  Naled  2.25  30  30  45  45  45
  Permethrin  43  10  10  30  30  30
  Prallethrin  0.05  -  -  60  60  -
  Pyrethrum  15  15  30  45  45  30
  Sumethrin  20  10  45  30  45  30 ")

  rv <- reactiveValues(data = insecticide_data , orig=insecticide_data)

  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    
    validate(need(ext == "csv", "Please upload a csv file"))
    
    rv$orig <- read.csv(file$datapath, header = input$header, )
    rv$data <- rv$orig
  })
  
  output$selectUI<-renderUI({
    req(rv$data)
    selectInput(inputId='selectcolumn', label='select column', choices = names(rv$data))
  })
  
  #splitcolumn
  observeEvent(input$Splitcolumn, {
    rv$data <- splitColumn(rv$data, input$selectcolumn)
  })
  
  #delterows
  observeEvent(input$deleteRows,{
    if (!is.null(input$table1_rows_selected)) {
      rv$data <- rv$data[-as.numeric(input$table1_rows_selected),]
    }
  })
  
  
  # renderDT ----------------------------------------------------------------
  
  output$table1 <- renderDT({
    datatable(rv$data, editable = TRUE)
  })
  
  observeEvent(input$table1_cell_edit, {
    row  <- input$table1_cell_edit$row
    clmn <- input$table1_cell_edit$col
    rv$data[row, clmn] <- input$table1_cell_edit$value
  })
  
  
  observeEvent(input$replacevalues, {
    rv$data <- fillvalues(rv$data, input$textbox, input$selectcolumn)
  })
  observeEvent(input$removecolumn, {
    rv$data <- removecolumn(rv$data,input$selectcolumn)
  })
  observeEvent(input$Undo, {
    rv$data <- rv$orig
  })
  
  # Abott ----------------------------------------------------------------
  
  Abbott = reactive({
    sample = input$sample
    dead_0 = sample[1,2] + sample[1,3] + sample[1,4] + sample[1,5]
    dead_5 = sample[2,2] + sample[2,3] + sample[2,4] + sample[2,5]
    dead_10 = sample[3,2] + sample[3,3] + sample[3,4] + sample[3,5]
    dead_15 = sample[4,2] + sample[4,3] + sample[4,4] + sample[4,5]
    dead_30 = sample[5,2] + sample[5,3] + sample[5,4] + sample[5,5]
    dead_45 = sample[6,2] + sample[6,3] + sample[6,4] + sample[6,5]
    dead_60 = sample[7,2] + sample[7,3] + sample[7,4] + sample[7,5]
    dead_75 = sample[8,2] + sample[8,3] + sample[8,4] + sample[8,5]
    dead_90 = sample[9,2] + sample[9,3] + sample[9,4] + sample[9,5]
    dead_105 = sample[10,2] + sample[10,3] + sample[10,4] + sample[10,5]
    dead_120 = sample[11,2] + sample[11,3] + sample[11,4] + sample[11,5]
    b1m_dead = sample[1,2] + sample[2,2] + sample[3,2] + sample[4,2] + sample[5,2] + sample[6,2] + sample[7,2] + sample[8,2] + sample[9,2] + sample[10,2] + sample[11,2]
    b2m_dead = sample[1,3] + sample[2,3] + sample[3,3] + sample[4,3] + sample[5,3] + sample[6,3] + sample[7,3] + sample[8,3] + sample[9,3] + sample[10,3] + sample[11,3]
    b3p_dead = sample[1,4] + sample[2,4] + sample[3,4] + sample[4,4] + sample[5,4] + sample[6,4] + sample[7,4] + sample[8,4] + sample[9,4] + sample[10,4] + sample[11,4]
    b4p_dead = sample[1,5] + sample[2,5] + sample[3,5] + sample[4,5] + sample[5,5] + sample[6,5] + sample[7,5] + sample[8,5] + sample[9,5] + sample[10,5] + sample[11,5]
    total_dead = b1m_dead + b2m_dead + b3p_dead + b4p_dead
    percent =  - c(dead_0/(total_dead-dead_0), dead_5/(total_dead-dead_5), dead_10/(total_dead-dead_10), dead_15/(total_dead-dead_15), dead_30/(total_dead-dead_30), dead_45/(total_dead-dead_45), dead_60/(total_dead-dead_60), dead_75/(total_dead-dead_75), dead_90/(total_dead-dead_90), dead_105/(total_dead-dead_105), dead_120/(total_dead-dead_120))
    time = c(sample[1,1], sample[2,1], sample[3,1], sample[4,1], sample[5,1], sample[6,1], sample[7,1], sample[8,1], sample[9,1], sample[10,1], sample[11,1])
    df <- data.frame(time,percent)
    graph1 = ggplot(df, aes(x = time, y = percent, group = 1)) + 
      geom_line(col='red') + 
      geom_smooth(method=lm, level=0.80)
    #graph1 = plot(time, percent, type = "b", pch = 19, col = "red", xlab = "Time (min)", ylab = "Percent Mortality")
    print(graph1)
    })
  
  output$plot = renderPlot({
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
