library(shiny)
library(ggplot2)
library(shinyFeedback)

ui <- fluidPage(
  
  useShinyFeedback(),
  
  titlePanel("Motor Learning Performance Curve"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons("performanceOrder", "Order of Performance:", 
                   choices = c("1", "2"), 
                   selected = NULL),
      
      numericInput("preTest", "Pre-test Score:", 
                   value = NA, min = 0, max = 15),
      
      numericInput("block1", "Block 1 Average Score:", 
                   value = NA, min = 0, max = 15),
      
      numericInput("block2", "Block 2 Average Score:", 
                   value = NA, min = 0, max = 15),
      
      numericInput("block3", "Block 3 Average Score:", 
                   value = NA, min = 0, max = 15),
      
      numericInput("block4", "Block 4 Average Score:", 
                   value = NA, min = 0, max = 15),
      
      numericInput("block5", "Block 5 Average Score:", 
                   value = NA, min = 0, max = 15),
      
      numericInput("block6", "Block 6 Average Score:", 
                   value = NA, min = 0, max = 15),
      
      numericInput("postTest", "Post-test Score:", 
                   value = NA, min = 0, max = 15),
      
      numericInput("retentionTest", "Retention Test Score:", 
                   value = NA, min = 0, max = 15),
      
      actionButton("submit", "Submit"),
      
      actionButton("reset", "Reset"),
      
      downloadButton("downloadData", "Download Data")
      
    ),
    
    mainPanel(
      
      plotOutput("performancePlot"),
      
      fluidRow(
        
        column(3, dateInput("dateStart", 
                            "Start Date:", 
                            value = Sys.Date() - 30)),
        
        column(3, dateInput("dateEnd", 
                            "End Date:", 
                            value = Sys.Date())),
        
        column(6, textInput("dataFilter", 
                            "Filter Data:", 
                            placeholder = "Enter keyword to filter data"))
        
      ),
      
      tableOutput("dataTable")
      
    )
  )
)

server <- function(input, output, session) {
  
  data_update_trigger <- reactiveVal(0)
  
  data <- reactive({
    
    data_update_trigger()
    
    if(!exists("data")) {
      data <- data.frame(
        id = character(),
        order = character(),
        preTest = numeric(),
        block1 = numeric(),
        block2 = numeric(),
        block3 = numeric(),
        block4 = numeric(),
        block5 = numeric(),
        block6 = numeric(),
        postTest = numeric(),
        retentionTest = numeric(),
        date = as.Date(character())
      )
    }
    
    data
    
  })
  
  new_id <- reactive({
    generateRandomID()
  })
  
  generateRandomID <- function() {
    paste0(sample(letters, 8), collapse = "") 
  }
  
  observeEvent(input$submit, {
    
    validate(
      need(!is.na(input$preTest), "Please enter a pre-test score"),
      need(!is.na(input$block1), "Please enter block 1 score"),
      need(!is.na(input$block2), "Please enter block 2 score"),
      need(!is.na(input$block3), "Please enter block 3 score"),
      need(!is.na(input$block4), "Please enter block 4 score"),
      need(!is.na(input$block5), "Please enter block 5 score"),
      need(!is.na(input$block6), "Please enter block 6 score"),
      need(!is.na(input$postTest), "Please enter post-test score"),
      need(!is.na(input$retentionTest), "Please enter retention test score")
    )
    
    new_data <- data.frame(
      id = new_id(),
      order = input$performanceOrder,
      preTest = input$preTest,
      block1 = input$block1,
      block2 = input$block2,
      block3 = input$block3,
      block4 = input$block4,
      block5 = input$block5,
      block6 = input$block6,  
      postTest = input$postTest,
      retentionTest = input$retentionTest,
      date = Sys.Date()
    )
    
    data(cbind(data, new_data))
    
    updateNumericInput(session, "preTest", value = NA)
    updateNumericInput(session, "block1", value = NA)
    updateNumericInput(session, "block2", value = NA)
    updateNumericInput(session, "block3", value = NA)
    updateNumericInput(session, "block4", value = NA)
    updateNumericInput(session, "block5", value = NA)
    updateNumericInput(session, "block6", value = NA)
    updateNumericInput(session, "postTest", value = NA)
    updateNumericInput(session, "retentionTest", value = NA)
    
    data_update_trigger(data_update_trigger() + 1)
    
  })
  
  output$dataTable <- renderTable({
    
    filtered_data <- data()
    
    if(!is.null(input$dateStart)) {
      filtered_data <- filtered_data[filtered_data$date >= input$dateStart, ]
    }
    
    if(!is.null(input$dateEnd)) {
      filtered_data <- filtered_data[filtered_data$date <= input$dateEnd, ]
    }
    
    if(!is.null(input$dataFilter) && input$dataFilter != "") {
      filtered_data <- filtered_data[grepl(input$dataFilter, filtered_data$id),]
    }
    
    head(filtered_data, 10)
    
  })
  
  output$performancePlot <- renderPlot({
    
    filtered_data <- data()
    
    if(nrow(filtered_data) == 0) {
      return()
    }
    
    ggplot(data = filtered_data, aes(x = order, y = preTest)) +
      geom_point() +
      geom_line()
    
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
  observeEvent(input$reset, {
    updateNumericInput(session, "preTest", value = NA)
    updateNumericInput(session, "block1", value = NA)
    updateNumericInput(session, "block2", value = NA)
    updateNumericInput(session, "block3", value = NA)
    updateNumericInput(session, "block4", value = NA)
    updateNumericInput(session, "block5", value = NA)
    updateNumericInput(session, "block6", value = NA)
    updateNumericInput(session, "postTest", value = NA)
    updateNumericInput(session, "retentionTest", value = NA)
  })
  
}

shinyApp(ui, server)