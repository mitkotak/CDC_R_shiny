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


m <- matrix(runif(6), 15, 6, dimnames = list(NULL, c("Time",
                                                       "Bottle 1M Dead",
                                                       "Bottle 2M Dead",
                                                       "Bottle 3P Dead",
                                                       "Bottle 4P Dead",
                                                       "Control Dead")))

m[,1] = c(0,5,10,15,20,25,30,35,40,45,60,75,90,105,120)
m[,2] = c(0,0,5,13,13,16,18,18,23,23,23,23,23,24,24)
m[,3] = c(0,0,5,13,13,16,18,18,23,23,23,23,23,24,24)
m[,4] = c(0,0,5,13,13,16,18,18,23,23,23,23,23,24,24)
m[,5] = c(0,0,5,13,13,16,18,18,23,23,23,23,23,24,24)
m[,6] = c(0,0,5,13,13,16,18,18,23,23,23,23,23,24,24)

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
                                 position="left",
                                 width=4,
                                 tags$h3("Data"),
                                 matrixInput("sample",
                                             value=m,
                                             rows=list(extend= TRUE),
                                             cols=list(names=TRUE),
                                             class='numeric')
                               ),
                               sidebarPanel(
                                 position="right",
                                 width=8,
                                 tags$h3("Resistance Plots"),
                                 plotOutput("plot")))),
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
                    # tabPanel("Data Upload", "Data Input", fluid=TRUE,
                    #          sidebarLayout(
                    #            sidebarPanel(
                    #              fileInput('file1', 'CSV File', accept= c('test/csv', 'text,comma-separated-values,text/plain', ' .csv')),
                    #              radioButtons('sep', 'Separator', c(Comma=',',Semicolon=';', Tab='\t'),','),
                    #              textInput("missing", "Missing Value Designator", value=NULL),
                    #              checkboxInput('header', 'Header', TRUE),
                    #              checkboxInput('show', 'Show all Rows', TRUE)),
                    #              #numericInput("Variable1", "Mortality in Test Bottle[%]:", value=NULL),
                    #              #numericInput("Variable2", "Mortality in Control Bottle[%]:", value=NULL),
                    #            mainPanel(tableOutput("contents"))),
                    #           position=c("left")),
                    # 
                    # tabPanel("Statistics", fluid=TRUE,
                    #          sidebarLayout(
                    #            sidebarPanel(
                    #              actionButton("stat", "Run Analysis"),
                    #              downloadButton("down3", "Download Table")),
                    #            mainPanel(tableOutput("results"))),
                    #          position=c("left")),
                    # 
                    # tabPanel("Density Plots", fluid=TRUE,
                    #          sidebarLayout(
                    #            sidebarPanel(
                    #              uiOutput("checkbox2"),
                    #              downloadButton("down1", "Download Plot")),
                    #            mainPanel(plotlyOutput("density")),
                    #          position=c("left"))),
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
    col_sums = colSums(sample[, c(2,3,4,5)])
    b1m_dead = col_sums[[1]]
    b2m_dead = col_sums[[2]]
    b3p_dead = col_sums[[3]]
    b4p_dead = col_sums[[4]]
    total_alive = b1m_dead + b2m_dead + b3p_dead + b4p_dead
    row_sums = rowSums(sample[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), c(2,3,4,5)])
    print(sample[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), c(2,3,4,5)])
    print(row_sums)
    Mortality_per = (100/total_alive)*c(
                row_sums[1],
                row_sums[2],
                row_sums[3],
                row_sums[4],
                row_sums[5],
                row_sums[6],
                row_sums[7],
                row_sums[8],
                row_sums[9],
                row_sums[10],
                row_sums[11],
                row_sums[12],
                row_sums[13],
                row_sums[14],
                row_sums[15])
    Time = c(sample[1,1], sample[2,1], sample[3,1], sample[4,1], sample[5,1], sample[6,1], sample[7,1], sample[8,1], sample[9,1], sample[10,1], sample[11,1], sample[12,1], sample[13,1], sample[14,1], sample[15,1])
    df <- data.frame(Time=Time,
                     Mortality=Mortality_per,
                     Resistance_Abolished=0.5*Time,
                     Resistance_Partially_Abolished=Time^0.6,
                     Resistance_Unaffected=(5*Time)/(Time+1))
    df <- df %>% pivot_longer(cols=c('Mortality', 'Resistance_Abolished', 'Resistance_Partially_Abolished','Resistance_Unaffected'),
                              names_to='resistances',
                              values_to='Percent_Mortality')
    graph1 = ggplot(df, aes(x = Time, y=Percent_Mortality)) +
      geom_line(aes(color=resistances), linetype='twodash') +
      scale_color_manual(name='Resistance_Types', labels=c('Observed', 'Resistance_Abolished', 'Resistance_Partially_Abolished', 'Resistance_Unaffected'),
                         values=c('red', 'purple', 'steelblue', 'pink')) +
      xlim(Time[1], Time[15])
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
