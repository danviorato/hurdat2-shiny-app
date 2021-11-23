source("R-scripts/librerias.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Base de datos de huracanes: HURDAT2"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            uiOutput("periodoUI"),
            uiOutput("statusUI"),
            textOutput("borrar")
        ,width = 4),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distTotal")
            )
        ),
    sidebarLayout(
        sidebarPanel(
            textAreaInput("borrar","borrar2")
        ),
        mainPanel(
            plotOutput("compvipe")
        )
    )
    )
    )
