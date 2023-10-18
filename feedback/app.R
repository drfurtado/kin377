# Load the Shiny package
library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)

# Initialize global reactive value
scoreDataBlocks <- reactiveVal(data.frame())

# Define UI
ui <- fluidPage(
    titlePanel("KIN 377 Feedback Lab"),
    
    div(
        sidebarLayout(
            sidebarPanel(
                selectInput("group", "Select Group:", choices = c("Please select a group" = "", "Delay-0" = 1, "Delay-2" = 2), selected = NULL),
                lapply(1:8, function(i) {
                    textInput(paste0("block", i), paste("Number of touchdowns for Block ", i), value = "")
                }),
                actionButton("submit", "Submit Scores")
            ),
            
            mainPanel(
                tags$p("Purpose: Test the efficacy of delayed feedback while learning a motor skill."),
                tags$p("Warning: Data are ONLY available while the app is running."),
                tags$ul(
                    tags$li("Select your group."),
                    tags$li("Each block represents 3 trials."),
                    tags$li("Enter the number of touchdowns for each block of 3 trials, either 0, 1, 2, or 3")
                ),
                plotOutput("scorePlot"),
                downloadButton('downloadData', 'Download Data'),
                
                # Add table output
                selectInput("n_entries", "Number of entries to view:", choices = c(10, 25, 50, 100), selected = 10),
                tableOutput("dataTable")
            )
        ),
        
        tags$footer(HTML("<hr><p style='font-size: small; text-align: center;'>Developed by <a href='https://drfurtado.us'>Ovande Furtado Jr.</a><br> 
                 Licensed under <a href='https://creativecommons.org/licenses/' target='_blank'>Creative Commons</a></p>"))
    )
)

# Define Server Logic
server <- function(input, output, session) {
    
    observeEvent(input$submit, {
        # Validation for group selection
        if (is.null(input$group) || input$group == "") {
            showModal(modalDialog(
                title = "Error",
                "Please select a group before submitting scores.",
                easyClose = TRUE
            ))
            return(NULL)
        }
        
        output$downloadData <- downloadHandler(
            filename = function() {
                paste('data-', Sys.Date(), '.csv', sep='')
            },
            content = function(file) {
                data_to_save <- as.data.frame(scoreDataBlocks())
                write.csv(data_to_save, file, row.names = FALSE)
            }
        )
        
        # Validate each block input for min and max values
        for (i in 1:8) {
            block_values <- as.numeric(unlist(strsplit(isolate(input[[paste0("block", i)]]), ",")))
            if (any(block_values < 0 | block_values > 3, na.rm = TRUE)) {
                showModal(modalDialog(
                    title = "Validation Error",
                    paste("Block ", i, " values should be between 0 and 3."),
                    easyClose = TRUE
                ))
                return(NULL)
            }
        }
        
        newScores <- sapply(1:8, function(i) {
            sum(as.numeric(unlist(strsplit(isolate(input[[paste0("block", i)]]), ","))), na.rm = TRUE)
        })
        
        randomID <- sample(100000:999999, 1)
        selectedGroup <- as.numeric(isolate(input$group))
        
        newRow <- data.frame(random_id = randomID, Group = selectedGroup, block1 = newScores[1], block2 = newScores[2], block3 = newScores[3], block4 = newScores[4], block5 = newScores[5], block6 = newScores[6], block7 = newScores[7], block8 = newScores[8])
        
        if (nrow(scoreDataBlocks()) == 0) {
            scoreDataBlocks(newRow)
        } else {
            updatedData <- rbind(scoreDataBlocks(), newRow)
            scoreDataBlocks(updatedData)
        }
        
        # Display submission message and reset form
        showModal(modalDialog(
            title = "Success",
            "Data successfully submitted.",
            easyClose = TRUE
        ))
        
        # Reset form
        lapply(1:8, function(i) {
            updateTextInput(session, paste0("block", i), value = "")
        })
    })
    
    output$scorePlot <- renderPlot({
        data <- scoreDataBlocks()
        if (!is.null(data) && nrow(data) > 0) {
            data_long <- tidyr::pivot_longer(data, cols = starts_with("block"), names_to = "Block", values_to = "Frequency")
            data_aggregated <- data_long %>% group_by(Block, Group) %>% summarise(Frequency = mean(Frequency, na.rm = TRUE))
            ggplot(data_aggregated, aes(x = Block, y = Frequency, group = Group, color = as.factor(Group))) +
                geom_line() +
                geom_point() +
                labs(title = "Frequency of Scores Across Blocks of Trials",
                     x = "Blocks of Trials",
                     y = "Frequency") +
                scale_color_manual(name = "Group", values = c("1" = "red", "2" = "blue"))
        }
    })
    
    # Render data table
    output$dataTable <- renderTable({
        data <- scoreDataBlocks()
        if (!is.null(data) && nrow(data) > 0) {
            head(data, n = as.numeric(input$n_entries))
        }
    })
    
}

# Run the App
shinyApp(ui = ui, server = server)
