source("R-scripts/librerias.R")

hurdat <- read_rds("data/hurdat2.rds")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Base de datos de huracanes: HURDAT2"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(
                "fecha_total", "Periodo", 
                start = min(hurdat$fecha), end = max(hurdat$fecha),
                min = min(hurdat$fecha), max = max(hurdat$fecha),
                separator = " - "
            ),
            checkboxGroupInput(
                "estatus_total", label = "Estatus", 
                choices = unique(hurdat$estatus), inline = T,
                selected = unique(hurdat$estatus)
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distTotal")
            )
        )
    )
    )