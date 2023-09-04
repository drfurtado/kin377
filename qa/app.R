library(shiny)

ui <- fluidPage(
    titlePanel("Q&A App with Local CSV Storage"),
    sidebarLayout(
        sidebarPanel(
            textInput("question", "Enter your question:"),
            actionButton("submit", "Submit")
        ),
        mainPanel(
            h3("Answer:"),
            verbatimTextOutput("answer"),
            h3("Previous Questions and Answers:"),
            tableOutput("table")
        )
    )
)

server <- function(input, output) {
    output$table <- renderTable({
        if (file.exists("questions_answers.csv")) {
            read.csv("questions_answers.csv")
        } else {
            data.frame(Question = character(0), Answer = character(0))
        }
    })
    
    observeEvent(input$submit, {
        question <- input$question
        answer <- paste("You asked:", question)
        
        new_entry <- data.frame(Question = question, Answer = answer)
        write.table(new_entry, file = "questions_answers.csv", append = TRUE,
                    quote = FALSE, sep = ",", col.names = FALSE, row.names = FALSE)
        
        output$answer <- renderText({ answer })
    })
}

shinyApp(ui, server)
