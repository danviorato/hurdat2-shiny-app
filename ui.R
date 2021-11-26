source("R-scripts/load-data.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Base de datos de huracanes: HURDAT2"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            materialSwitch("switchUI", 
                           value = T),
            conditionalPanel(#Investigar conditional panel
                condition = "input.switchUI",
                uiOutput("periodoUI")
            ),
            conditionalPanel(
                condition = "!input.switchUI",
                uiOutput("fechaUI")
            ),
            uiOutput("statusUI")
        ,width = 4),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distTotal")
            )
        ),
    sidebarLayout(
        sidebarPanel(
            sliderTextInput(
                "statusvientoSer", label = "Status",
                choices = c("All", estatusc), selected = "All" 
            )
        ),
        mainPanel(
            plotOutput("compvipe")
        )
    ),
    fluidRow(
#        verbatimTextOutput("prueba"),
#        tableOutput("prueba2")
    )
    )
    )
