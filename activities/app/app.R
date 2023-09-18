library(shiny)

# List of names
names <- c(
    "Aceves, Isabella", "Alvarado, Andrew", "Andrade, Hayly", "Cardenas, Javier",
    "Cisneros, Nicole", "Cokas, Mari", "Cruz, Samantha", "Dearo, Melody", 
    "Derderian, Vana", "Flores, Zoey", "Fonseca, Laylah", "Garcia Morales, Keylin",
    "Gharabeki, Kevin", "Gonzalez, Miguel", "Gutierrez, Samuel", "Hernandez, Eloisa", 
    "Jackson, John", "Joo, Jaesung", "Legaspi, Patrick", "Lowey, Katie", 
    "Luna, Michael", "Mazariego, Ana", "Molina, Maya", "Nierenberg, Sean", 
    "Nuno Sanchez, Freddy", "Ortega, Remy", "Perez, Alex", "Perez, Diego", 
    "Pollard, Aliyah", "Rangel, Matthew", "Reyes, Elizabeth", "Santana, Bianca", 
    "Shaffer, Megan", "Shahverdi, Eric", "Shori, Varun", "Smith, Nathaniel", 
    "Susanto, Matthew", "Velez Alarcon, Jocelyn", "Zavala, Kimberlee"
)

# UI
ui <- fluidPage(
    titlePanel("Random Name Selector"),
    sidebarLayout(
        sidebarPanel(
            actionButton("select_name", "Select a random name")
        ),
        mainPanel(
            textOutput("name_output")
        )
    )
)

# Server
server <- function(input, output) {
    observeEvent(input$select_name, {
        output$name_output <- renderText({
            sample(names, 1)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
