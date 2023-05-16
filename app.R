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
library(shinyscreenshot)
library(samplingbook)



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


## Reads in user input data
m <- matrix(c(0,0,0,0,0,0,
              5,1,0,0,0,0,
              10,5,1,0,0,0,
              15,13,3,0,0,0,
              20,15,5,0,0,0,
              25,16,7,5,9,0,
              30,17,9,6,12,0,
              35,18,13,8,16,0,
              40,20,15,10,18,0,
              45,21,17,12,22,0,
              60,22,22,22,24,1,
              75,23,23,22,24,2,
              90,23,23,22,24,2,
              105,24,24,22,24,3,
              120,24,24,24,24,24), 15, 6, byrow=TRUE, dimnames = list(NULL, c("Time",
                                                       "Bottle1 Dead",
                                                       "Bottle2 Dead",
                                                       "Bottle3 Dead",
                                                       "Bottle4 Dead",
                                                       "Control Dead")))

## Diagnostic times from CDC Manual
diagtimes = matrix(c(20, 45, 45, 90, 45, 60, 0.75, 30, 30, 45, 60, 0, 12.5, 15, 30, 15, 30, 60, 800, 0, 0, 75, 45,
                     45, 400, 15, 30, 45, 45, 45, 2.25, 30, 30, 45, 45, 45, 43, 10, 10, 30, 30, 30, 0.05, 0, 0, 60, 60,
                     0, 15, 15, 30, 45, 45, 30, 20, 10, 45, 30, 45, 30), 10, 6, byrow=TRUE)
rownames(diagtimes) = c("Chlorpyrifos", "Deltamethrin", "Etofenprox", "Fenthion", "Malathion", "Naled", "Permethrin", "Prallethrin", "Pyrethrum", "Sumethrin")

colnames(diagtimes)= c("Insecticide Conc.", "Ae. aegypti", "Ae. albopictus", "Cx. pipens", "Cx. quinquefasciatus", "Cx. tarsalis")

# Define UI for application that draws a histogram
ui <- fluidPage(theme= shinytheme("yeti"),
                  tabsetPanel(
                    tabPanel("Insecticide Mortality", fluid=TRUE,
                             fluidRow(
                               column(
                               12,
                               htmlOutput("instructions"))
                             ),
                             fluidRow(
                               column(
                                 12,
                                 htmlOutput("step1"))),
                             fluidRow(
                               column(
                                 2,
                                 selectInput('insecticides', "Insecticides", c(None="", rownames(diagtimes)))
                               ),
                               column(
                                 2,
                                 selectInput('species', "Species", c(None="", colnames(diagtimes)[2:6]))
                               ),
                               column(
                                 8,
                                 htmlOutput("datacheck")
                               )
                             ),
                             fluidRow(
                               column(
                                 6,
                                 htmlOutput("step2")
                               ),
                               column(
                                 5,
                                 htmlOutput("recommendation")
                               )
                             ),
                             fluidRow(
                               column(
                                 4,
                                 span(tags$h4("Data"), style="color:blue; font-family:arial; font-style:italic"),
                                 matrixInput("sample",
                                             value=m,
                                             rows=list(names=FALSE),
                                             cols=list(names=TRUE),
                                             class='numeric')
                               ),
                               column(
                                 8,
                                 span(tags$h4("Resistance Plots"), style="color:blue; font-family:arial; font-style:italic"),
                                 plotOutput("plot"))
                             ),
                             fluidRow(
                               column(
                                 2,
                                 actionButton("go", "Take a screenshot")
                               ),
                               column(
                                 10,
                                 htmlOutput("resistantstate")
                               )
                             )),
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
                )
                    )

# Define server function
server <- function(session, input, output) {
  library(tidyverse)
  library(tibble)
  rv <- reactiveValues(data = diagtimes , orig=diagtimes)

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
  
  Datacheck = reactive({
    req(input$insecticides)
    req(input$species)
    data = as.data.frame(input$sample)
    mortality = data
    for(c in 2:ncol(data)){mortality[,c]=data[,c]/data[nrow(data),c]} #calculate %mortality based on final count of live and dead
    
    #data check
    #enter warning here if control mortality >10%
    if(mortality$Control[(nrow(data)-1)]>0.1){
      paste("<br>Warning: mortality in control bottle is too high. Please repeat bioassay.")
    }
    else{
      paste("<br>No inconsitencies detected in mortality data.")
    }
  })
  
  output$datacheck = renderText({
    Datacheck()
  })

  Abbott = reactive({
    req(input$insecticides)
    req(input$species)
    data = as.data.frame(input$sample)
    mortality = data
    for(c in 2:ncol(data)){mortality[,c]=data[,c]/data[nrow(data),c]} #calculate %mortality based on final count of live and dead

    #Abbot's
    if(mortality$Control[(nrow(data)-1)]>0.03){ #use Abbot's
      for(c in 2:(ncol(data)-1)){
        mortality[,c]=(mortality[,c]-mortality[,ncol(data)])/(1-mortality[,ncol(data)])
      }}

    diag_time <<- diagtimes[input$insecticides, input$species]
    print(input$insecticides)
    print(input$species)
    print(diag_time)
    print(diagtimes)
    conc <<- diagtimes[input$insecticides, 1]

    mortality$Median=apply(mortality[,2:(ncol(data)-1)],1,median,na.rm=T)
    mortality_long=pivot_longer(mortality[,1:(ncol(data)-1)],cols = 2:(ncol(data)-1),names_to = "Replicate",values_to = "Mortality")
    color_breaks <- data.frame(start = c(0, 0.9, 0.97),  # Create data with breaks
                               end = c(0.9, 0.97, 1),
                               colors = c("#FF0000","#FF9900","#FFFF99"))
    
    graph1 = ggplot()+
    #plotting the guides
      geom_rect(aes(xmin=0,xmax=Inf,ymin=0,ymax=0.9),alpha=0.5,fill="red")+
      geom_rect(aes(xmin=0,xmax=Inf,ymin=0.9,ymax=0.97),alpha=0.5,fill="orange")+
      geom_rect(aes(xmin=0,xmax=Inf,ymin=0.97,ymax=1),alpha=0.5,fill="yellow")+
      geom_vline(xintercept = diag_time,color="blue",linetype="dotted")+ #diagnostic time line
    #plotting the observations
    #leaves off the final row to avoid including the live ones
    #individual replicates
      geom_line(data=mortality_long[which(mortality_long$Time<max(data$Time)),], 
                aes(x=Time,y=Mortality,group=Replicate),color="white",linetype="dashed")+
    #median of all replicates
      geom_line(data=mortality[1:(nrow(data)-1),], 
                aes(x=Time,y=Median))+
    #labels
      ylab("Percent Mortality")+xlab("Time (minutes)")+
      theme_minimal()

    rv$state = TRUE
    mortality_locked <<- mortality
    print(graph1)
    })
  
  output$plot = renderPlot({
    Abbott()
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
  
  ResistantState = reactive({
    req(rv$state)
    df = mortality_locked[1:(nrow(as.data.frame(input$sample))-1),]
    observed_per <<- df[df$Time == diag_time, ]$Median
    rv$state2 = TRUE
    if (observed_per > 0.97){
      paste("<font color=\"#FF0000\"><u><b>SNo resistance detected (population susceptible): </b> Consider baseline mechanism testing (enzymes, molecular assays, or CDC bottle bioassay with inhibitors). <b>Continue monitoring.</b></u></font>")
    }
    else if( (observed_per >= 0.90) || (observed_per <= 0.96)){
      paste("<font color=\"#FF9900\"><u><b>Developing resistance:</b> Consider mechanism testing (enzymes, molecular assays, or CDC bottle bioassay with inhibitors) and field testing.<b> Rotate insecticide products and implem entintegrated pest management best practices.</b></u></font>")
    }
    else{
      paste("<font color=\"#FFFF99\"><u><b>Resistant:</b> Consider intensity testing (looking at mortality at 120 minutes or CDC bottle bioassay with 1X, 2X, 5X, and 10X the diagnostic dosage of insecticide ), mechanism testing ( enzymes, molecular assays, or CDC bottle bioassay with inhibitors) and field testing. Avoid this insecticide in this population</u></font>")
      }
    })
  
  output$resistantstate = renderText({
   ResistantState()
  })
  
  
  Recommendation = reactive({
    req(rv$state2)
    req(input$insecticides)
    req(input$species)
    n_rec = sample.size.prop(e=0.1*observed_per, P=observed_per, N=Inf, level=0.80)[[2]]
    paste("<font color=\"#ff69b4\"><u>It is recommended that you have", n_rec,
          "mosquitoes, treat each bottle with", conc,"ug of insecticide and track mortality for a minimum of", diag_time,
          "minutes</u></font>")
  })
  
  output$recommendation = renderText({
    Recommendation()
  })
  
  observeEvent(input$go, {
    screenshot()
  })
  
  output$instructions = renderText({
    paste("The steps below will help you to analyze data from the CDC bottle bioassay according to the <u><a href=\"https://www.cdc.gov/mosquitoes/pdfs/CONUS-508.pdf\">CONUS Manual for Evaluating Insecticide Resistance in Mosquitoes Using the CDC Bottle Bioassay Kit[PDF – 19 pages]</a></u><br>
          Programs in the continental United States and its territories can order free Insecticide Resistance Kits by sending an email to USBottleAssayKit@cdc.gov and requesting an order form. Kits include bottles, insecticide, and manual.")
  })
  output$step1 = renderText({
    paste("<font color=\"#ff00ff\"; font-family:times><b><br>Step 1: CDC has determined bottle bioassay threshold times and diagnostic doses for several species of mosquitoes. Select the mosquito species and insecticide from the dropdown menus below to set your threshold and dose. (NOTE: If you wish to change the diagnostic time for your calculations, you may do so on the Insecticide Diagnostic Times tab above)</b></font><br>")
  })
  
  output$step2 = renderText({
    print("<font color=\"#ff00ff\"; font-family:times><b>Step 2: Enter the number of dead mosquitos at each time point in each bottle into the table. At the final time point, enter the total number of mosquitoes (dead and alive) in the bottles. It may help in counting to freeze the bottle at the end of the experiment to knock down all the mosquitoes</b></font>")
  })

} #server

# Run the application 
shinyApp(ui = ui, server = server)
