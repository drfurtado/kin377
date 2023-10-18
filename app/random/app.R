library(shiny)
library(ggplot2)
library(shinyFeedback)

ui <- fluidPage(
    useShinyFeedback(),  # add this line
    titlePanel("Motor Learning Performance Curve"),
    
    sidebarLayout(
        sidebarPanel(
            radioButtons("performanceOrder", "Order of Performance:", choices = c("1", "2"), selected = NULL),
            numericInput("preTest", "Pre-test Score:", value = NULL, min = 0, max = 15),
            numericInput("block1", "Block 1 Average Score:", value = NULL, min = 0, max = 15),
            numericInput("block2", "Block 2 Average Score:", value = NULL, min = 0, max = 15),
            numericInput("block3", "Block 3 Average Score:", value = NULL, min = 0, max = 15),
            numericInput("block4", "Block 4 Average Score:", value = NULL, min = 0, max = 15),
            numericInput("block5", "Block 5 Average Score:", value = NULL, min = 0, max = 15),
            numericInput("block6", "Block 6 Average Score:", value = NULL, min = 0, max = 15),
            numericInput("postTest", "Post-test Score:", value = NULL, min = 0, max = 15),
            numericInput("retentionTest", "Retention Test Score:", value = NULL, min = 0, max = 15),
            actionButton("submit", "Submit"),
            actionButton("reset", "Reset"),
            downloadButton("downloadData", "Download Data")
            
        ),
        
        mainPanel(
            plotOutput("performancePlot"),
            fluidRow(
                column(3, dateInput("dateStart", "Start Date:", value = Sys.Date() - 30)),
                column(3, dateInput("dateEnd", "End Date:", value = Sys.Date())),
                column(6, textInput("dataFilter", "Filter Data:", placeholder = "Enter keyword to filter data"))
            ),
            tableOutput("dataTable")
        )
    )
)

generateRandomID <- function() {
    return(paste0(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""))
}

server <- function(input, output, session) {
    
    data_update_trigger <- reactiveVal(0)
    
    data_reactive <- reactive({
        data_update_trigger()
        
        if (file.exists("data.csv")) {
            data <- read.csv("data.csv", stringsAsFactors = FALSE)
            
            # Print the structure and a snapshot of the data at this stage
            print("Data after reading from CSV:")
            str(data)
            print(head(data))
            
            # Extract the date part of the Timestamp column for filtering
            data$DateTemp <- as.Date(format(as.POSIXct(data$Timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"), tz="UTC"))
            
            
            # Apply date filter
            start_date <- as.Date(input$dateStart)
            end_date <- as.Date(input$dateEnd)
            
            new_data <- data[data$DateTemp >= start_date & data$DateTemp <= end_date, ]
            new_data$DateTemp <- NULL  # Remove the DateTemp column here
            
            # Print the structure and a snapshot of the data at this stage
            print("Data after date filter:")
            str(new_data)
            print(head(new_data))
            
            # Apply keyword filter
            if (nchar(input$dataFilter) > 0 && nrow(new_data) > 0) {
                new_data <- new_data[apply(new_data, 1, function(row) any(grepl(input$dataFilter, row, ignore.case = TRUE))),]
            }
            
            # Print the structure and a snapshot of the data at this stage
            print("Data after keyword filter:")
            str(new_data)
            print(head(new_data))
            
            # Remove the DateTemp column before returning new_data
            new_data$DateTemp <- NULL  
            
            if (nrow(new_data) == 0) {
                return(data.frame(ID=integer(), Timestamp=character(), PerformanceOrder=character(), PreTest=numeric(), Block1=numeric(), Block2=numeric(), Block3=numeric(), Block4=numeric(), Block5=numeric(), Block6=numeric(), PostTest=numeric(), RetentionTest=numeric(), stringsAsFactors = FALSE))
            } else {
                return(new_data)
            }
            
            
        } else {
            return(data.frame())
        }
    })
    
    
    
    output$dataTable <- renderTable({
        data <- data_reactive()
        if(nrow(data) > 0) {
            tail(data, 10)
        } else {
            data.frame()
        }
    })
    
    observeEvent(input$submit, {
        
        # Validation to ensure all fields are filled
        validate(
            need(!is.na(input$preTest) & !is.null(input$preTest), "Please enter a pre-test score"),
            need(!is.na(input$block1) & !is.null(input$block1), "Please enter a score for block 1"),
            need(!is.na(input$block2) & !is.null(input$block2), "Please enter a score for block 2"),
            need(!is.na(input$block3) & !is.null(input$block3), "Please enter a score for block 3"),
            need(!is.na(input$block4) & !is.null(input$block4), "Please enter a score for block 4"),
            need(!is.na(input$block5) & !is.null(input$block5), "Please enter a score for block 5"),
            need(!is.na(input$block6) & !is.null(input$block6), "Please enter a score for block 6"),
            need(!is.na(input$postTest) & !is.null(input$postTest), "Please enter a post-test score"),
            need(!is.na(input$retentionTest) & !is.null(input$retentionTest), "Please enter a retention test score")
        )
        
        
        scores <- c(input$preTest, input$block1, input$block2, input$block3, input$block4, input$block5, input$block6, input$postTest, input$retentionTest)
        
        
        timestamp <- Sys.time()
        randomID <- generateRandomID()
        
        newData <- data.frame(
            ID = randomID, 
            Timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"), 
            PerformanceOrder = input$performanceOrder,
            PreTest = input$preTest, 
            Block1 = input$block1, 
            Block2 = input$block2, 
            Block3 = input$block3, 
            Block4 = input$block4, 
            Block5 = input$block5, 
            Block6 = input$block6, 
            PostTest = input$postTest, 
            RetentionTest = input$retentionTest, 
            stringsAsFactors = FALSE
        )
        
        
        if (file.exists("data.csv")) {
            oldData <- read.csv("data.csv", stringsAsFactors = FALSE)
            data <- rbind(oldData, newData)
        } else {
            data <- newData
        }
        
        write.table(data, file = "data.csv", row.names = FALSE, sep = ",")
        data_update_trigger(data_update_trigger() + 1)
    })
    
    output$performancePlot <- renderPlot({
        data <- data_reactive()
        
        if(nrow(data) > 0) {
            # Specify the columns to include in the colMeans function, excluding the "PerformanceOrder" column
            score_cols <- 4:ncol(data)
            
            # Calculate column means
            avgData <- data.frame(Trial = 1:9, Score = colMeans(data[, score_cols], na.rm = TRUE))
            trialLabels <- c("Pre-test", "Block 1", "Block 2", "Block 3", "Block 4", "Block 5", "Block 6", "Post-test", "Retention Test")
            
            ggplot(avgData, aes(x = Trial, y = Score, group = 1)) +
                geom_line(alpha = 0.5) +
                geom_point() +
                scale_x_continuous(breaks = 1:9, labels = trialLabels) +
                labs(title = "Performance Curve", x = "Trials", y = "Score") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
        } else {
            ggplot() + 
                labs(title = "No Data Available", x = "Trials", y = "Score") +
                theme_minimal()
        }
    })
    
    
    
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(data_reactive(), file)
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
shinyApp(ui = ui, server = server)
