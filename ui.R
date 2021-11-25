source("R-scripts/librerias.R")

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
                condition = "input.materialSwitch",
                uiOutput("periodoUI")
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
            uiOutput("statusvientoUI")
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
