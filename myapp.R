library(shiny)
library(reshape2)
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



# UI ----------------------------------------------------------------------

ui <- fluidPage(
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
      actionButton("Undo", 'Undo'),
      downloadButton("downloadData", "Download")
    ),
    mainPanel(
      DTOutput("table1")
    )
  )
)


# SERVER ------------------------------------------------------------------

server <- function(session, input, output) {
  rv <- reactiveValues(data = read.csv("insecticide_diag_time.csv"), orig=read.csv("insecticide_diag_time.csv"))

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
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(rv$data, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$data(), file, row.names = FALSE)
    }
  )

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
}

shinyApp(ui, server)