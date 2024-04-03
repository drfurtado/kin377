library(shiny)

# Load flashcards data
flashcards <- read.csv("flashcards.csv")

# Define UI
ui <- fluidPage(
    titlePanel("Flashcard App"),
    mainPanel(
        uiOutput("question"),
        uiOutput("answer"),
        actionButton("show", "Show Answer"),
        actionButton("next", "Next")
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive value to store current flashcard index
    current_index <- reactiveVal(1)
    
    # Display question
    output$question <- renderUI({
        current <- current_index()
        wellPanel(
            h3(flashcards$Question[current])
        )
    })
    
    # Display answer
    output$answer <- renderUI({
        current <- current_index()
        if(input$show > 0) {
            wellPanel(
                h4(flashcards$Answer[current])
            )
        }
    })
    
    # Next button logic
    observeEvent(input$next, {
        current <- current_index()
        if (current < nrow(flashcards)) {
            current_index(current + 1)
        } else {
            current_index(1) # Loop back to the first card
        }
        # Reset show button
        shinyjs::reset("show")
    })
    
    # Automatically hide the answer when moving to the next question
    observe({
        current_index() # Dependency
        isolate(shinyjs::hide("answer"))
    })
}

# Run the app
shinyApp(ui = ui, server = server)
